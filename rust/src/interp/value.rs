use crate::{
    interp::env::LEnv,
    parser::ast::{Block, Name},
};
use std::{
    cell::RefCell,
    cmp::Ordering::{self, *},
    collections::HashMap,
    fmt::{Display, Formatter},
    ops::{Add, Mul, Neg, Sub},
    rc::Rc,
};

// Le type des valeurs de l'interpréteur.
// Celui-ci (ainsi que de nombreux autres types dans l'interpréteur) dépend
// d'un paramètre de lifetime 'ast représentant la durée de vie des emprunts de
// valeurs provenant de l'AST du programme. L'AST reste alloué et gelé pendant
// toute l'exécution de l'interpréteur, ce qui permet à cette durée de vie de
// rester vivante pendant toute l'exécution de l'interpréteur.
#[derive(Clone)]
pub enum Value<'ast> {
    Nil,
    Bool(bool),
    Number(Number),
    String(Rc<String>),
    Function(Function<'ast>),
    Table(Rc<RefCell<Table<'ast>>>),
}

// Type des nombres utilisées dans un programme mini-lua
#[derive(Debug, Clone, Copy)]
pub enum Number {
    Integer(i64),
    Float(f64),
}
use Number::*;

// Les fonctions sont soit la primitive "print", soit une fermeture,
// contenant une liste de noms de paramètres, un environnement local et une
// référence vers un bloc de l'AST constituant le corps de la fermeture.
#[derive(Clone)]
pub enum Function<'ast> {
    Print,
    Closure(&'ast [Name], Rc<LEnv<'ast>>, &'ast Block),
}

pub type Table<'ast> = HashMap<TKey, Value<'ast>>;

// Type des entrées de table de hachage
#[derive(Hash, PartialEq, Eq)]
pub enum TKey {
    KInt(i64),
    KString(Rc<String>),
}

// Fonctions privées sur le type Number. Elle ne sont utilisées que dans le
// module courant.
impl Add<Number> for Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Number {
        match (self, rhs) {
            (Integer(n1), Integer(n2)) => Integer(n1 + n2),
            (Integer(n1), Float(f2)) => Float(n1 as f64 + f2),
            (Float(f1), Integer(n2)) => Float(f1 + n2 as f64),
            (Float(f1), Float(f2)) => Float(f1 + f2),
        }
    }
}

impl Sub<Number> for Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Number {
        match (self, rhs) {
            (Integer(n1), Integer(n2)) => Integer(n1 - n2),
            (Integer(n1), Float(f2)) => Float(n1 as f64 - f2),
            (Float(f1), Integer(n2)) => Float(f1 - n2 as f64),
            (Float(f1), Float(f2)) => Float(f1 - f2),
        }
    }
}

impl Neg for Number {
    type Output = Number;
    fn neg(self) -> Number {
        match self {
            Integer(n) => Integer(-n),
            Float(f) => Float(f),
        }
    }
}

impl Mul<Number> for Number {
    type Output = Number;
    fn mul(self, rhs: Self) -> Number {
        match (self, rhs) {
            (Integer(n1), Integer(n2)) => Integer(n1 * n2),
            (Integer(n1), Float(f2)) => Float(n1 as f64 * f2),
            (Float(f1), Integer(n2)) => Float(f1 * n2 as f64),
            (Float(f1), Float(f2)) => Float(f1 * f2),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Equal)
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        fn if_cmp(n: i64, f: f64) -> Option<Ordering> {
            Some((n as f64).partial_cmp(&f)?.then_with(|| {
                if n < f.ceil() as i64 {
                    Less
                } else if n > f.floor() as i64 {
                    Greater
                } else {
                    Equal
                }
            }))
        }
        match (*self, *other) {
            (Integer(n1), Integer(n2)) => n1.partial_cmp(&n2),
            (Integer(n1), Float(f2)) => if_cmp(n1, f2),
            (Float(f1), Integer(n2)) => if_cmp(n2, f1).map(Ordering::reverse),
            (Float(f1), Float(f2)) => f1.partial_cmp(&f2),
        }
    }
}

// Le trait Display permet d'afficher une valeur lors de l'exécution de
// l'instruction print. Dans le code de l'interpréteur, on peut ainsi utiliser
// l'instruction print!("{}", v) pour afficher la valeur v.
impl<'ast> Display for Value<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => write!(f, "{}", b),
            Number(Float(x)) => write!(f, "{:?}", x),
            Number(Integer(x)) => write!(f, "{}", x),
            String(str) => write!(f, "{}", str),
            Function(..) => write!(f, "<function>"),
            Table(..) => write!(f, "<table>"),
        }
    }
}

// Les fonctions suivantes peuvent être utilisées dans l'interpréteur pour
// implémenter les fonctionnalités correspondantes pour mini-lua.

impl<'ast> Value<'ast> {
    pub fn as_bool(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Nil)
    }

    pub fn as_number(self) -> Number {
        if let Value::Number(res) = self {
            res
        } else {
            panic!("{} : not a number", self)
        }
    }

    pub fn as_function(self) -> Function<'ast> {
        if let Value::Function(res) = self {
            res
        } else {
            panic!("{} : not a function", self)
        }
    }

    pub fn as_table(self) -> Rc<RefCell<Table<'ast>>> {
        if let Value::Table(res) = self {
            res
        } else {
            panic!("{} : not a table", self)
        }
    }

    pub fn as_table_key(self) -> TKey {
        match self {
            Value::Number(Number::Integer(n)) => TKey::KInt(n),
            Value::String(s) => TKey::KString(s),
            _ => panic!("{} : not a table key (int or string)", self),
        }
    }

    pub fn add(self, other: Self) -> Self {
        Value::Number(self.as_number() + other.as_number())
    }

    pub fn sub(self, other: Self) -> Self {
        Value::Number(self.as_number() - other.as_number())
    }

    pub fn mul(self, other: Self) -> Self {
        Value::Number(self.as_number() * other.as_number())
    }

    pub fn neg(self) -> Self {
        Value::Number(-self.as_number())
    }

    pub fn lt(self, other: Self) -> bool {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => n1 < n2,
            (s, o) => panic!("trying to compare {} and {}", s, o),
        }
    }

    pub fn le(self, other: Self) -> bool {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => n1 <= n2,
            (s, o) => panic!("trying to compare {} and {}", s, o),
        }
    }
}

// L'opérateur == peut être utiliser pour tester l'égalité de deux valeurs.
impl<'ast> PartialEq for Value<'ast> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Function(f1), Value::Function(f2)) => std::ptr::eq(f1, f2),
            (Value::Table(t1), Value::Table(t2)) => std::ptr::eq(t1, t2),
            _ => false,
        }
    }
}
