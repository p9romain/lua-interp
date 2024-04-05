use super::{ast, internalast};
use itertools::Itertools;
use std::rc::Rc;

fn simp_block((ss, rs): internalast::Block) -> Result<ast::Stat, String> {
    if let Some(_) = rs {
        Err(format!("return before end of function"))
    } else {
        simp_stats(ss.into_iter())
    }
}

fn simp_stats<I: Iterator<Item = internalast::Stat>>(i: I) -> Result<ast::Stat, String> {
    i.map(simp_stat)
        .try_fold(Box::new(ast::Stat_::Nop), |acc, s| {
            Ok(Box::new(ast::Stat_::Seq(acc, s?)))
        })
}

fn simp_stat(stat: internalast::Stat) -> Result<ast::Stat, String> {
    use internalast::Stat::*;
    match stat {
        Assign(vs, es) => {
            if vs.len() != 1 || es.len() != 1 {
                return Err(format!("parallel assign"));
            }
            Ok(Box::new(ast::Stat_::Assign(
                simp_var(vs.into_iter().next().unwrap())?,
                simp_exp(es.into_iter().next().unwrap())?,
            )))
        }
        StatFunctionCall(fcall) => Ok(Box::new(ast::Stat_::StatFunctionCall(simp_fcall(fcall)?))),
        Break => Err(format!("break")),
        WhileDoEnd(cond, body) => Ok(Box::new(ast::Stat_::WhileDoEnd(
            simp_exp(cond)?,
            simp_block(body)?,
        ))),
        If(cases, elsecase) => cases.into_iter().rev().try_fold(
            elsecase.map_or_else(|| Ok(Box::new(ast::Stat_::Nop)), simp_block)?,
            |acc, (cond, blk)| {
                Ok(Box::new(ast::Stat_::If(
                    simp_exp(cond)?,
                    simp_block(blk)?,
                    acc,
                )))
            },
        ),
        Label(_) => Err(format!("label")),
        Goto(_) => Err(format!("goto")),
        DoEnd(_) => Err(format!("do .. end")),
        RepeatUntil(..) => Err(format!("repeat .. until")),
        ForStep(..) | ForIn(..) => Err(format!("for loop")),
        LocalAssign(_, None) => Err(format!("local variable declaration without a value")),
        LocalAssign(_, Some(_)) => Err(format!("inner local variable declaration")),
    }
}

fn simp_exp(exp: internalast::Exp) -> Result<ast::Exp, String> {
    use internalast::Exp_::*;
    let res = match *exp {
        Nil => ast::Exp_::Nil,
        False => ast::Exp_::False,
        True => ast::Exp_::True,
        Number(n) => ast::Exp_::Number(n),
        LiteralString(s) => ast::Exp_::LiteralString(Rc::new(s)),
        Var(v) => ast::Exp_::Var(simp_var(v)?),
        ExpFunctionCall(fcall) => ast::Exp_::ExpFunctionCall(simp_fcall(fcall)?),
        FunctionDef(fbody) => ast::Exp_::FunctionDef(simp_fbody(fbody)?),
        BinOp(op, e1, e2) => ast::Exp_::BinOp(simp_binop(op)?, simp_exp(e1)?, simp_exp(e2)?),
        UnOp(op, e) => ast::Exp_::UnOp(simp_unop(op)?, simp_exp(e)?),
        Table(t) => ast::Exp_::Table(simp_tableconstr(t)?),
        VarArg => return Err(format!("vararg")),
    };
    Ok(Box::new(res))
}

fn simp_tableconstr(tc: internalast::TableConstr) -> Result<Vec<(ast::Exp, ast::Exp)>, String> {
    let simp_kv = |(k, v)| {
        if let Some(k) = k {
            Ok((simp_exp(k)?, simp_exp(v)?))
        } else {
            Err(format!("tables constructors with implicit keys"))
        }
    };
    tc.into_iter().map(simp_kv).collect()
}

fn simp_binop(op: internalast::BinOp) -> Result<ast::BinOp, String> {
    use internalast::BinOp::*;
    match op {
        Addition => Ok(ast::BinOp::Addition),
        Subtraction => Ok(ast::BinOp::Subtraction),
        Multiplication => Ok(ast::BinOp::Multiplication),
        Equality => Ok(ast::BinOp::Equality),
        Inequality => Ok(ast::BinOp::Inequality),
        Less => Ok(ast::BinOp::Less),
        Greater => Ok(ast::BinOp::Greater),
        LessEq => Ok(ast::BinOp::LessEq),
        GreaterEq => Ok(ast::BinOp::GreaterEq),
        LogicalAnd => Ok(ast::BinOp::LogicalAnd),
        LogicalOr => Ok(ast::BinOp::LogicalOr),
        op => Err(format!("operator: {:?}", op)),
    }
}

fn simp_unop(op: internalast::UnOp) -> Result<ast::UnOp, String> {
    use internalast::UnOp::*;
    match op {
        UnaryMinus => Ok(ast::UnOp::UnaryMinus),
        LogicalNot => Ok(ast::UnOp::Not),
        op => Err(format!("operator: {:?}", op)),
    }
}

fn simp_var(v: internalast::Var) -> Result<ast::Var, String> {
    use internalast::Var::*;
    match v {
        Name(n) => Ok(ast::Var::Name(n)),
        IndexTable(tbl, k) => Ok(ast::Var::IndexTable(simp_exp(tbl)?, simp_exp(k)?)),
    }
}

fn simp_fcall(fc: internalast::FunctionCall) -> Result<ast::FunctionCall, String> {
    use internalast::FunctionCall::*;
    match fc {
        Function(e, args) => Ok(ast::FunctionCall(
            simp_exp(e)?,
            args.into_iter().map(simp_exp).collect::<Result<_, _>>()?,
        )),
        Method(..) => Err(format!("method call")),
    }
}

fn simp_fbody((mut ns, _, blk): internalast::FuncBody) -> Result<ast::FuncBody, String> {
    if ns.get(0).map(|s| s.as_str()) == Some("self") {
        ns.remove(0);
    }
    Ok((ns, simp_outerblock(blk)?))
}

pub fn simp_outerblock((stats, ret): internalast::Block) -> Result<ast::Block, String> {
    let n_locals = stats
        .iter()
        .take_while(|s| matches!(s, internalast::Stat::LocalAssign(..)))
        .count();
    let mut it = stats.into_iter();
    let locals = (&mut it)
        .take(n_locals)
        .map(|stat| {
            if let internalast::Stat::LocalAssign(ns, None) = stat {
                Ok(ns)
            } else {
                Err(format!("local variable declaration with initialization"))
            }
        })
        .flatten_ok()
        .unique()
        .collect::<Result<Vec<String>, String>>()?;
    let body = simp_stats(it)?;
    let ret = match ret {
        None => Box::new(ast::Exp_::Nil),
        Some(v) if v.len() <= 1 => v
            .into_iter()
            .next()
            .map_or_else(|| Ok(Box::new(ast::Exp_::Nil)), simp_exp)?,
        Some(_) => return Err(format!("return with multiple values")),
    };
    Ok(ast::Block { locals, body, ret })
}
