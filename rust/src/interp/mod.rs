use self::{
  env::{Env, GEnv, LEnv},
  value::{Function, Value},
};
use crate::parser::ast::*;
use std::{collections::HashMap, rc::Rc};

mod env;
pub mod value;

impl Block {
  // Interprétation d'un bloc
  fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
    // todo : self.locals
    self.body.interp(env) ;
    self.ret.interp(env)
  }
}

impl Stat_ {
  // Interprétation d'une instruction
  fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) {
    match self {
      Self::Nop => (),
      Self::Seq(stat1, stat2) => {
        stat1.interp(env) ;
        stat2.interp(env)
      }
      Stat_::Assign(var, expr) => {
        match var {
          Var::Name(name) => {
            let value = expr.interp(env) ;
            env.set(name, value)
          },
          Var::IndexTable(_, _) => todo!(),
        }
      },
      Self::StatFunctionCall(function) => {
        match function.interp(env) {
          Value::Nil => (),
          ret => panic!("{} : not nil", ret)
        }
      },
      Stat_::WhileDoEnd(_, _) => todo!(),
      Stat_::If(_, _, _) => todo!(),
    }
  }
}

impl FunctionCall {
  // Interprétation d'un appel de fonction
  fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
    match self.0.interp(env)
    {
      Value::Function(function) => {
        match function {
          Function::Print => {
            let to_print = self.1
              .iter()
              .map(|expr| expr.interp(env))
              .fold("".to_owned(), |acc, value| {
                  if acc == "" { format!("{}", value) }
                  else { format!("{}\t{}", acc, value) }
                }
              ) ;
            println!("{}", to_print) ;
            Value::Nil
          },
          Function::Closure(_, _, _) => unimplemented!()
        }
      },
      val => panic!("{} is not a function name (string)", val),
    }
  }
}

impl Exp_ {
  // Interprétation d'une expression
  fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
    match self {
        Exp_::Nil => Value::Nil,
        Exp_::False => Value::Bool(false),
        Exp_::True => Value::Bool(true),
        Exp_::Number(number) => Value::Number(*number),
        Exp_::LiteralString(string) => Value::String(string.clone()),
        Exp_::Var(var) => {
          match var {
            Var::Name(name) => env.lookup(name),
            Var::IndexTable(_, _) => todo!()
          }
        },
        Exp_::ExpFunctionCall(_) => todo!(),
        Exp_::FunctionDef(_) => todo!(),
        Exp_::BinOp(bop, lhs, rhs) => {
          match bop {
            BinOp::Addition => lhs.interp(env).add(rhs.interp(env)),
            BinOp::Subtraction => lhs.interp(env).sub(rhs.interp(env)),
            BinOp::Multiplication => lhs.interp(env).mul(rhs.interp(env)),
            BinOp::Equality => Value::Bool(lhs.interp(env) == rhs.interp(env)),
            BinOp::Inequality => Value::Bool(lhs.interp(env) != rhs.interp(env)),
            BinOp::Less => Value::Bool(lhs.interp(env).lt(rhs.interp(env))),
            BinOp::Greater => Value::Bool(!lhs.interp(env).le(rhs.interp(env))),
            BinOp::LessEq => Value::Bool(lhs.interp(env).le(rhs.interp(env))),
            BinOp::GreaterEq => Value::Bool(!lhs.interp(env).lt(rhs.interp(env))),
            BinOp::LogicalAnd => todo!(),
            BinOp::LogicalOr => todo!(),
          }
        },
        Exp_::UnOp(uop, expr) => {
          match uop {
            UnOp::UnaryMinus => expr.interp(env).neg(),
            UnOp::Not => Value::Bool(!expr.interp(env).as_bool()),
          }
        },
        Exp_::Table(_) => todo!(),
    }
  }
}

// Point d'entrée principal de l'interpréteur
pub fn run(ast: &Block) {
  let mut globals = GEnv(HashMap::new());
  let printid = "print".to_owned();
  globals.0.insert(&printid, Value::Function(Function::Print));
  let mut env = Env {
    locals: Rc::new(LEnv::Nil),
    globals: &mut globals,
  };
  ast.interp(&mut env);
}
