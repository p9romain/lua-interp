use self::{
  env::{Env, GEnv, LEnv},
  value::{Function, Value},
};
use crate::parser::ast::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod env;
pub mod value;

impl Block {
  // Interprétation d'un bloc
  fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> 
  {
    // local variables inside the function
    let mut local_values = Vec::new() ;
    for _ in 0..self.locals.len() {
      local_values.push(Value::Nil) ;
    } ;

    // adding the local varaibles inside the current environment 
    // (only in this scope !)
    let mut n_env = Env {
        locals: env.locals.extend(&self.locals, local_values.into_iter()),
        globals: env.globals,
    } ;

    self.body.interp(&mut n_env) ;
    self.ret.interp(&mut n_env)
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
          Var::IndexTable(table, key) => {
            let table = table.interp(env).as_table() ;
            let key = key.interp(env).as_table_key() ;
            let value = expr.interp(env) ;
            let _ = table.clone()
              .borrow_mut()
              .insert(key, value) ;
          }
        }
      },
      Self::StatFunctionCall(function) => {
        match function.interp(env) {
          Value::Nil => (),
          ret => panic!("{} : not nil", ret)
        }
      },
      Stat_::WhileDoEnd(expr, stat) => {
        while expr.interp(env).as_bool() {
          stat.interp(env)
        }
      },
      Stat_::If(expr, stat_then, stat_else) => {
        if expr.interp(env).as_bool() { stat_then.interp(env) }
        else { stat_else.interp(env) }
      },
    }
  }
}

impl FunctionCall {
  // Interprétation d'un appel de fonction
  fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> 
  {
    match self.0.interp(env).as_function()
    {
      Function::Print => {
        let print = self.1
          .iter()
          .map(|expr| expr.interp(env))
          .fold("".to_owned(), 
            |acc, value| {
              if acc == "" { format!("{value}") }
              else { format!("{acc}\t{value}") }
            }
          ) ;
        println!("{print}") ;
        Value::Nil
      },
      Function::Closure(args, lenv, block) => {
        let lack_of_args =
          if self.1.len() < args.len() { args.len() - self.1.len() }
          else { 0 } ;
        let arg_values = self.1
          .iter()
          .map(|expr| expr.interp(env))
          // padding
          .chain(vec![Value::Nil; lack_of_args]) ;
        // adding the values in the env in order to acces them
        let mut n_env = Env {
            locals: lenv.extend(args, arg_values),
            globals: env.globals,
        } ;
        block.interp(&mut n_env)
      }
    }
  }
}

impl Exp_ {
  // Interprétation d'une expression
  fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> 
  {
    match self {
        Exp_::Nil => Value::Nil,
        Exp_::False => Value::Bool(false),
        Exp_::True => Value::Bool(true),
        Exp_::Number(number) => Value::Number(*number),
        Exp_::LiteralString(string) => Value::String(string.clone()),
        Exp_::Var(var) => {
          match var {
            Var::Name(name) => env.lookup(name),
            Var::IndexTable(table, key) => {
              let table = table.interp(env).as_table() ;
              let key = key.interp(env).as_table_key() ;
              table.clone()
                .borrow()
                .get(&key)
                .map_or(Value::Nil, |value| value.clone())
            }
          }
        },
        Exp_::ExpFunctionCall(function) => function.interp(env),
        Exp_::FunctionDef(function) => {
          Value::Function(
            Function::Closure(
              &function.0, 
              env.locals.clone(), 
              &function.1
            )
          )
        },
        Exp_::BinOp(bop, lhs, rhs) => {
          let interp_lhs = lhs.interp(env) ; 
          match bop {
            // because of the lazyness
            BinOp::LogicalAnd => {
              if interp_lhs.as_bool() { rhs.interp(env) }
              else { interp_lhs }
            },
            BinOp::LogicalOr => {
              if interp_lhs.as_bool() { interp_lhs }
              else { rhs.interp(env) }
            },
            _ => {
              let interp_rhs = rhs.interp(env) ;
              match bop {
                BinOp::Addition => interp_lhs.add(interp_rhs),
                BinOp::Subtraction => interp_lhs.sub(interp_rhs),
                BinOp::Multiplication => interp_lhs.mul(interp_rhs),
                BinOp::Equality => Value::Bool(interp_lhs == interp_rhs),
                BinOp::Inequality => Value::Bool(interp_lhs != interp_rhs),
                BinOp::Less => Value::Bool(interp_lhs.lt(interp_rhs)),
                BinOp::Greater => Value::Bool(!interp_lhs.le(interp_rhs)),
                BinOp::LessEq => Value::Bool(interp_lhs.le(interp_rhs)),
                BinOp::GreaterEq => Value::Bool(!interp_lhs.lt(interp_rhs)),
                _ => unreachable!()
              }
            }
          }
        },
        Exp_::UnOp(uop, expr) => {
          let expr = expr.interp(env) ;
          match uop {
            UnOp::UnaryMinus => expr.neg(),
            UnOp::Not => Value::Bool(!expr.as_bool()),
          }
        },
        Exp_::Table(items) => {
          let mut table = HashMap::new() ;
          for (key, value) in items.iter() {
            let _ = table.insert(
              key.interp(env)
                .as_table_key(),
              value.interp(env)
            ) ;
          }
          Value::Table(Rc::new(RefCell::new(table)))
        },
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
