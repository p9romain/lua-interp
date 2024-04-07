open Luaparser.Ast
type value = Value.t
type env = Value.env

(* Fonction auxiliaire pour créer une table d'environnement à partir de noms et
   valeurs associées. *)
let create_scope (names: string list) 
                 (values: value list) : (name, value) Hashtbl.t =
  let values = ref values in
  let items = List.to_seq 
    @@ List.map (
      fun (name : string) : (name * value) ->
        match !values with
        | [] -> name, Nil
        | value :: next ->
          values := next ;
          name, value
    )
    names 
  in
  Hashtbl.of_seq items

(* Fonctions de l'interprète, mutuellement récursives. Une fonction par
   catégorie syntaxique de l'AST. *)

(* Interprète un bloc de code *)
let rec interp_block (env : env) 
                     (blk : block) : value =
  let local_values = create_scope blk.locals [] in
  let n_env = Value.{ env with locals = local_values :: env.locals } in
  let () = interp_stat n_env blk.body in
  interp_exp n_env blk.ret

(* Interprète un statement *)
and interp_stat (env : env) 
                (stat : stat) : unit =
  match stat with
  | Nop -> ()
  | Seq (stat, stat') ->
    interp_stat env stat ;
    interp_stat env stat'
  | Assign (var, expr) -> (
    match var with
    | Name name -> Value.set_ident env name @@ interp_exp env expr
    | IndexTable (table, key) ->
      let table = Value.as_table @@ interp_exp env table in
      let key = Value.as_table_key @@ interp_exp env key in
      let value = interp_exp env expr in
      Hashtbl.replace table key value
  )
  | FunctionCall func -> (
    match interp_funcall env func with
    | Nil -> ()
    | _ -> failwith "function should return nil"
  )
  | WhileDoEnd (expr, stat) ->
    while Value.as_bool @@ interp_exp env expr do
      interp_stat env stat
    done
  | If (expr, stat, stat') ->
    if Value.as_bool @@ interp_exp env expr then
      interp_stat env stat
    else
      interp_stat env stat'

(* Interprète un appel de fonction *)
and interp_funcall (env : env) 
                   (fc : functioncall) : value =
  let (func, values) = fc in
  let values = List.map (
    fun (expr : exp) : value ->
      interp_exp env expr 
  ) 
  values
  in
  match Value.as_function @@ interp_exp env func with
  | Print ->
    let () = Printf.printf "%s\n" 
    @@ String.concat "\t" 
    @@ List.map Value.to_string values
    in
    Nil
  | Closure (args, lenv, block) ->
    let scope = create_scope args values in
    let n_env = Value.{ env with locals = scope :: lenv.locals } in
    interp_block n_env block

(* Interprète une expression *)
and interp_exp (env : env) 
               (e : exp) : value =
  match e with
  | Nil -> Nil
  | False -> Bool false
  | True -> Bool true
  | Integer n -> Int n
  | Float f -> Float f
  | LiteralString s -> String s
  | Var var -> (
    match var with
    | Name name -> Value.lookup_ident env name
    | IndexTable (table, key) ->
      let table = Value.as_table @@ interp_exp env table in
      let key = Value.as_table_key @@ interp_exp env key in
      Option.value (Hashtbl.find_opt table key) ~default:Nil
  )
  | FunctionCallE func -> interp_funcall env func
  | FunctionDef (args, block) ->
    Function(
      Closure(
        args,
        env,
        block
      )
    )
  | BinOp (bop, lhs, rhs) -> (
    let interp_lhs = interp_exp env lhs in
    match bop with
    | LogicalAnd ->
      if Value.as_bool interp_lhs then
        interp_exp env rhs
      else
        interp_lhs
    | LogicalOr ->
      if Value.as_bool interp_lhs then
        interp_lhs
      else
        interp_exp env rhs
    | _ -> (
      let interp_rhs = interp_exp env rhs in
      match bop with
      | Addition -> Value.add interp_lhs interp_rhs
      | Subtraction -> Value.sub interp_lhs interp_rhs
      | Multiplication -> Value.mul interp_lhs interp_rhs
      | Equality -> Bool (Value.equal interp_lhs interp_rhs)
      | Inequality -> Bool (not @@ Value.equal interp_lhs interp_rhs)
      | Less -> Bool (Value.lt interp_lhs interp_rhs)
      | Greater -> Bool (not @@ Value.le interp_lhs interp_rhs)
      | LessEq -> Bool (Value.le interp_lhs interp_rhs)
      | GreaterEq -> Bool (not @@ Value.lt interp_lhs interp_rhs)
      | _ -> assert false (* unreachable *)
    )
  )
  | UnOp (uop, expr) -> (
    let expr = interp_exp env expr in
    match uop with
    | UnaryMinus -> Value.neg expr
    | Not -> Bool (not @@ Value.as_bool expr)
  )
  | Table items -> 
    let items = List.to_seq
      @@ List.map (
        fun (key, value : exp * exp) : (Value.tkey * value) ->
          let key = Value.as_table_key @@ interp_exp env key in
          let value = interp_exp env value in
          key, value
      )
    items
    in
    Table (Hashtbl.of_seq items)

let run ast =
  let globals = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Value.Function Print);
  let env = Value.{ globals; locals = [] } in
  ignore (interp_block env ast)
