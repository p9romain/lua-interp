open Luaparser.Ast
type value = Value.t
type coroutine = Value.coroutine
type env = Value.env

let rec interp_block (env : env) 
                     (blk : block) 
                     (k : value -> unit) : unit =
  assert false

and interp_stat (env : env) 
                (stat : stat) 
                (k : unit -> unit) : unit =
  match stat with
  | FunctionCall func -> (
    interp_funcall env func (
      fun (ret_value : value) : unit ->
        match ret_value with
        | Nil -> k ()
        | _ -> failwith "function should return nil"
    )
  )
  | _ -> assert false

and interp_funcall (env : env) 
                   (func, values : functioncall) 
                   (k: value -> unit) : unit =
  let values = List.map (
    fun (expr : exp) : value ->
      interp_exp env expr
  ) 
  values
  in
  interp_exp env func (
    fun (func : value) : unit ->
      match Value.as_function func with
      | Print ->
        let () = Printf.printf "%s\n" 
          @@ String.concat "\t" 
          @@ List.map Value.to_string values
        in
        k Nil
      | _ -> assert false
  )

and interp_exp (env : env) 
               (e : exp) 
               (k: value -> unit) : unit =
  match e with
  | Nil -> k @@ Nil
  | False -> k @@ Bool false
  | True -> k @@ Bool true
  | Integer n -> k @@ Int n
  | Float f -> k @@ Float f
  | LiteralString s -> k @@ String s
  | Var var -> (
    match var with
    | Name name -> k @@ Value.lookup_ident env name
    | IndexTable (table, key) ->
      interp_exp env table (
        fun (table : value) : unit ->
          interp_exp env key (
            fun (key : value) : unit ->
              let table = Value.as_table table in
              let key = Value.as_table_key key in
              k @@ Option.value (Hashtbl.find_opt table key) ~default:Nil
          )
      )
  )
  | FunctionCallE func -> interp_funcall env func k
  | FunctionDef (args, block) ->
    k @@ Function(
      Closure(
        args,
        env,
        block
      )
    )
  | BinOp (bop, lhs, rhs) -> (
    interp_exp env lhs (
      fun (interp_lhs : value) : unit ->
        match bop with
        | LogicalAnd ->
          if Value.as_bool interp_lhs then
            interp_exp env rhs k
          else
            k @@ interp_lhs
        | LogicalOr ->
          if Value.as_bool interp_lhs then
            k @@ interp_lhs
          else
            interp_exp env rhs k
        | _ ->
          interp_exp env rhs (
            fun (interp_rhs : value) : unit ->
              match bop with
              | Addition -> k @@ Value.add interp_lhs interp_rhs
              | Subtraction -> k @@ Value.sub interp_lhs interp_rhs
              | Multiplication -> k @@ Value.mul interp_lhs interp_rhs
              | Equality -> k @@ Bool (Value.equal interp_lhs interp_rhs)
              | Inequality -> k @@ Bool (not @@ Value.equal interp_lhs interp_rhs)
              | Less -> k @@ Bool (Value.lt interp_lhs interp_rhs)
              | Greater -> k @@ Bool (not @@ Value.le interp_lhs interp_rhs)
              | LessEq -> k @@ Bool (Value.le interp_lhs interp_rhs)
              | GreaterEq -> k @@ Bool (not @@ Value.lt interp_lhs interp_rhs)
              | _ -> assert false (* unreachable *)
          )
    )
  )
  | UnOp (uop, expr) -> (
    interp_exp env expr (
      fun (expr : value) : unit ->
        match uop with
        | UnaryMinus -> k @@ Value.neg expr
        | Not -> k @@ Bool (not @@ Value.as_bool expr)
    )
  )
  | Table _ -> assert false
    (* let items = List.to_seq
      @@ List.map (
        fun (key, value : exp * exp) : (Value.tkey * value) ->
          let key = Value.as_table_key @@ interp_exp env key in
          let value = interp_exp env value in
          key, value
      )
    items
    in
    Table (Hashtbl.of_seq items) *)

let run ast =
  let coroutine : (Value.tkey, value) Hashtbl.t = Hashtbl.create 4 in
  Hashtbl.add coroutine (KString "create") (Value.Function CoroutCreate);
  Hashtbl.add coroutine (KString "yield")  (Value.Function CoroutYield);
  Hashtbl.add coroutine (KString "mini_resume") (Value.Function CoroutResume);
  Hashtbl.add coroutine (KString "status") (Value.Function CoroutStatus);
  let globals : (string, value) Hashtbl.t = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Function Print);
  Hashtbl.add globals "coroutine" (Table coroutine);
  let env = Value.{ globals; locals = [] } in

  interp_block env ast (fun _ -> ())
