exception Unhandled_ast of string

module SSet = Set.Make(String)

let rec simp_block ((ss, rs) : Internalast.block) : Ast.stat =
  if rs <> None then
    raise (Unhandled_ast "return before end of function");
  simp_stats ss

and simp_stats (l : Internalast.stat list) : Ast.stat =
  let fold (acc : Ast.stat) (s : Internalast.stat) : Ast.stat =
    Seq (acc, simp_stat s)
  in
  List.fold_left fold Nop l

and simp_stat : Internalast.stat -> Ast.stat = function
  | Assign ([v], [e]) -> Assign (simp_var v, simp_exp e)
  | Assign (_, _) -> raise (Unhandled_ast "parallel assign")
  | FunctionCall fcall -> FunctionCall (simp_fcall fcall)
  | Break -> raise (Unhandled_ast "break")
  | WhileDoEnd (cond, body) -> WhileDoEnd (simp_exp cond, simp_block body)
  | If (cases, elsecase) ->
    List.fold_right
      (fun (cond, blk) acc -> Ast.If (simp_exp cond, simp_block blk, acc))
      cases
      (Option.fold ~none:Ast.Nop ~some:simp_block elsecase)
  | Label _ -> raise (Unhandled_ast "label")
  | Goto _ -> raise (Unhandled_ast "goto")
  | DoEnd blk -> simp_block blk
  | RepeatUntil _ -> raise (Unhandled_ast "repeat .. until")
  | ForStep _ | ForIn _  -> raise (Unhandled_ast "for loop")
  | LocalAssign _ ->
    raise (Unhandled_ast "inner local variable declaration")

and simp_exp : Internalast.exp -> Ast.exp = function
  | Nil -> Nil
  | False -> False
  | True -> True
  | Integer n -> Integer n
  | Float f -> Float f
  | LiteralString s -> LiteralString s
  | Var v -> Var (simp_var v)
  | FunctionCallE fcall -> FunctionCallE (simp_fcall fcall)
  | FunctionDef fbody -> FunctionDef (simp_fbody fbody)
  | BinOp (op, e1, e2) -> BinOp (simp_binop op, simp_exp e1, simp_exp e2)
  | UnOp (op, e) -> UnOp (simp_unop op, simp_exp e)
  | Table t -> Table (simp_tableconstr t)
  | Vararg -> raise (Unhandled_ast "vararg")

and simp_tableconstr : Internalast.tableconstr -> (Ast.exp * Ast.exp) list = fun l ->
  List.map (function
    | (Some k, v) -> (simp_exp k, simp_exp v)
    | _ -> raise (Unhandled_ast "tables constructors with implicit keys")
  ) l

and simp_binop : Internalast.binop -> Ast.binop = function
  | Addition -> Addition
  | Subtraction -> Subtraction
  | Multiplication -> Multiplication
  | Equality -> Equality
  | Inequality -> Inequality
  | Less -> Less
  | Greater -> Greater
  | LessEq -> LessEq
  | GreaterEq -> GreaterEq
  | LogicalAnd -> LogicalAnd
  | LogicalOr -> LogicalOr
  | op -> raise (Unhandled_ast ("operator: " ^ Internalast.show_binop op))

and simp_unop : Internalast.unop -> Ast.unop = function
  | UnaryMinus -> UnaryMinus
  | LogicalNot -> Not
  | op -> raise (Unhandled_ast ("operator: " ^ Internalast.show_unop op))

and simp_var : Internalast.var -> Ast.var = function
  | Name n -> Name n
  | IndexTable (tbl, k) -> IndexTable (simp_exp tbl, simp_exp k)

and simp_fcall : Internalast.functioncall -> Ast.functioncall = function
  | Function (e, args) -> (simp_exp e, List.map simp_exp args)
  | Method _ -> raise (Unhandled_ast "method call")

and simp_fbody : Internalast.funcbody -> Ast.funcbody =
  fun (ns, _, blk) ->
  let ns =
    match ns with
    | "self" :: ns -> ns
    | _ -> ns
  in
  ns, simp_outerblock blk

and simp_outerblock (blk : Internalast.block) : Ast.block =
  let rec split locals : Internalast.stat list -> Ast.name list * Internalast.stat list = function
  | LocalAssign (_, Some _) :: _  ->
    raise (Unhandled_ast "local variable declaration with initialization")
  | LocalAssign (ns, None) :: q ->
    let locals = List.fold_left (Fun.flip SSet.add) locals ns in
    split locals q
  | body -> (SSet.elements locals, body)
  in
  let locals, body = split SSet.empty (fst blk) in
  let body = simp_stats body in
  let ret : Ast.exp =
    match snd blk with
    | None | Some [] -> Nil
    | Some [e] -> simp_exp e
    | Some (_::_) -> raise (Unhandled_ast "return with multiple values")
  in
  { locals; body; ret }
