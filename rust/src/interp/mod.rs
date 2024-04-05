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
      Self::StatFunctionCall(function) => {
        match function.interp(env) {
          Value::Nil => (),
          ret => panic!("{} : not nil", ret)
        }
      },
      _ => unimplemented!()
    }
  }
}

impl FunctionCall {
  // Interprétation d'un appel de fonction
  fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
    match self.0.interp(env)
    {
      Value::String(function_name) => {
        match env.lookup(function_name.as_ref()).as_function() {
          Function::Print => {
            let mut to_print = "".into() ;
            for v in self.1.iter().map(|expr| expr.interp(env)) {
              to_print = format!("{}\t{}", to_print, v) ;
            }
            println!("{}", to_print.trim_start()) ;
            Value::Nil
          },
          _ => unimplemented!()
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
        Exp_::Number(n) => Value::Number(*n),
        Exp_::LiteralString(s) => Value::String(s.clone()),
        Exp_::Var(v) => {
          match v {
            Var::Name(s) => Value::String(Rc::new(s.to_string())),
            _ => unimplemented!()
        }
        },
        _ => unimplemented!()
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
