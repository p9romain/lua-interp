use std::rc::Rc;

#[derive(Debug)]
pub struct Block {
    pub locals: Vec<Name>,
    pub body: Stat,
    pub ret: Exp,
}

#[derive(Debug)]
pub enum Stat_ {
    Nop,
    Seq(Stat, Stat),
    Assign(Var, Exp),
    StatFunctionCall(FunctionCall),
    WhileDoEnd(Exp, Stat),
    If(Exp, Stat, Stat),
}
pub type Stat = Box<Stat_>;

pub type FuncBody = (Vec<Name>, Block);

pub type Name = String;

#[derive(Debug)]
pub enum Exp_ {
    Nil,
    False,
    True,
    Number(Number),
    LiteralString(Rc<String>),
    Var(Var),
    ExpFunctionCall(FunctionCall),
    FunctionDef(FuncBody),
    BinOp(BinOp, Exp, Exp),
    UnOp(UnOp, Exp),
    Table(Vec<(Exp, Exp)>),
}
pub type Exp = Box<Exp_>;

pub type Number = crate::interp::value::Number;

#[derive(Debug)]
pub struct FunctionCall(pub Exp, pub Args);

#[derive(Debug)]
pub enum Var {
    Name(Name),
    IndexTable(Exp, Exp),
}

pub type Args = Vec<Exp>;

#[derive(Debug)]
pub enum BinOp {
    Addition,
    Subtraction,
    Multiplication,
    /* relational operators */
    Equality,
    Inequality,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    /* logical operators */
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub enum UnOp {
    UnaryMinus,
    Not,
}
