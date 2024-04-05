pub type Block = (Vec<Stat>, Option<RetStat>);

pub enum Stat {
    Assign(Vec<Var>, Vec<Exp>),
    StatFunctionCall(FunctionCall),
    Label(Name),
    Break,
    Goto(Name),
    DoEnd(Block),
    WhileDoEnd(Exp, Block),
    RepeatUntil(Block, Exp),
    If(Vec<(Exp, Block)>, Option<Block>),
    ForStep(Name, Exp, Exp, Option<Exp>, Block),
    ForIn(Vec<Name>, Vec<Exp>, Block),
    LocalAssign(Vec<Name>, Option<Vec<Exp>>),
}

pub type FuncBody = (Vec<Name>, Option<()>, Block);

pub type RetStat = Vec<Exp>;

pub type Name = String;

pub type Number = super::ast::Number;

pub enum Exp_ {
    Nil,
    False,
    True,
    Number(Number),
    LiteralString(String),
    Var(Var),
    ExpFunctionCall(FunctionCall),
    VarArg,
    FunctionDef(FuncBody),
    Table(TableConstr),
    BinOp(BinOp, Exp, Exp),
    UnOp(UnOp, Exp),
}
pub type Exp = Box<Exp_>;

pub type TableConstr = Vec<(Option<Exp>, Exp)>;

pub enum FunctionCall {
    Function(Exp, Args),
    Method(Exp, Name, Args),
}

pub enum Var {
    Name(Name),
    IndexTable(Exp, Exp),
}

pub type Args = Vec<Exp>;

#[derive(Debug)]
pub enum BinOp {
    /* arithmetic operators */
    Addition,
    Subtraction,
    Multiplication,
    FloatDivision,
    FloorDivision,
    Modulo,
    Exponentiation,
    /* bitwise operators */
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftRight,
    ShiftLeft,
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
    /* string concatenation */
    Concat,
}

#[derive(Debug)]
pub enum UnOp {
    UnaryMinus,
    BitwiseNot,
    Length,
    LogicalNot,
}
