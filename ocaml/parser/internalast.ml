type block = stat list * retstat option

and stat =
  | Assign           of var list * exp list
  | FunctionCall     of functioncall
  | Label            of name
  | Break
  | Goto             of name
  | DoEnd            of block
  | WhileDoEnd       of exp * block
  | RepeatUntil      of block * exp
  | If               of (exp * block) list * block option
  | ForStep          of name * exp * exp * exp option * block
  | ForIn            of name list * exp list * block
  | LocalAssign      of name list * exp list option

and funcbody = name list * unit option * block

and retstat = exp list

and name = string

and exp =
  | Nil
  | False
  | True
  | Integer        of Int64.t
  | Float          of float
  | LiteralString  of string
  | Var            of var
  | FunctionCallE  of functioncall
  | Vararg
  | FunctionDef    of funcbody
  | Table          of tableconstr
  | BinOp          of binop * exp * exp
  | UnOp           of unop * exp

and tableconstr = (exp option * exp) list

and functioncall =
  | Function of exp * args
  | Method   of exp * name * args

and var =
  | Name       of name
  | IndexTable of exp * exp

and args = exp list

and binop =
  (* arithmetic operators *)
  | Addition
  | Subtraction
  | Multiplication
  | FloatDivision
  | FloorDivision
  | Modulo
  | Exponentiation
  (* bitwise operators *)
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftRight
  | ShiftLeft
  (* relational operators *)
  | Equality
  | Inequality
  | Less
  | Greater
  | LessEq
  | GreaterEq
  (* logical operators *)
  | LogicalAnd
  | LogicalOr
  (* string concatenation *)
  | Concat

and unop =
  | UnaryMinus
  | BitwiseNot
  | Length
  | LogicalNot

[@@deriving show]
