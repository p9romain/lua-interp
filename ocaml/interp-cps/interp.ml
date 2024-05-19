open Luaparser.Ast
type value = Value.t
type coroutine = Value.coroutine
type env = Value.env

let interp_args (env : env) 
                (l : 'a list) 
                (f : env -> 'a -> ('b -> unit) -> unit)
                (k : 'b list -> unit) : unit =
    let rec loop (acc : 'b list) 
                 (l : 'a list) 
                 (k : 'b list -> unit) : unit =
      match l with
      | [] -> k (List.rev acc)
      | expr :: ll ->
        f env expr (
          fun (res : 'b) : unit ->
            loop (res :: acc) ll k
        )
    in
    loop [] l k

let create_scope (names: string list) 
                 (values: value list) : (name, value) Hashtbl.t =
  let values = ref values in
  let items = List.to_seq 
    @@ List.map (
      fun (name : string) : (name * value) ->
        match !values with
        | [] -> name, Nil (* Not enough given params *)
        | value :: next ->
          values := next ; 
          name, value
    )
    names 
  in
  Hashtbl.of_seq items

let rec interp_block (env : env) 
                     (blk : block) 
                     (k : value -> unit) : unit =
  let local_values = create_scope blk.locals [] in
  let n_env = Value.{ env with locals = local_values :: env.locals } in
  interp_stat n_env blk.body (
    fun (_ : unit) : unit ->
      interp_exp n_env blk.ret k
  )

and interp_stat (env : env) 
                (stat : stat) 
                (k : unit -> unit) : unit =
  match stat with
  | Nop -> k ()
  | Seq (stat, stat') ->
    interp_stat env stat (
      fun (_ : unit) : unit ->
        interp_stat env stat' k
    )
  | Assign (var, expr) -> (
    match var with
    | Name name ->
      interp_exp env expr (
        fun (value : value) : unit ->
          Value.set_ident env name value ;
          k ()
      )
    | IndexTable (table, key) ->
      interp_exp env table (
        fun (table : value) : unit ->
          interp_exp env key (
            fun (key : value) : unit ->
              interp_exp env expr (
                fun (value : value) : unit ->
                  let table = Value.as_table table in
                  let key = Value.as_table_key key in
                  Hashtbl.replace table key value ;
                  k ()
              )
          )
      )

  )
  | FunctionCall func -> (
    interp_funcall env func (
      fun (ret : value) : unit ->
        match ret with
        | Nil -> k ()
        | _ -> failwith "function should return nil"
    )
  )
  | WhileDoEnd (expr, stat) ->
    interp_exp env expr (
      fun (value : value) : unit ->
        if Value.as_bool value then
          interp_stat env stat (
            fun (_ : unit) : unit ->
              interp_stat env (WhileDoEnd (expr, stat)) k
          )
        else
          k ()
    )
  | If (expr, stat_then, stat_else) ->
    interp_exp env expr (
      fun (value : value) : unit ->
        if Value.as_bool value then
          interp_stat env stat_then k
        else
          interp_stat env stat_else k
    )

and interp_funcall (env : env) 
                   (func, values : functioncall) 
                   (k: value -> unit) : unit =
  interp_args env values interp_exp (
    fun (values : value list) : unit ->
      interp_exp env func (
        fun (func : value) : unit ->
          match Value.as_function func with
          | Print ->
            let () = Printf.printf "%s\n" 
              @@ String.concat "\t" 
              @@ List.map Value.to_string values
            in
            k Nil
          | Closure (args, lenv, block) ->
            let scope = create_scope args values in
            let n_env = Value.{ env with locals = scope :: lenv.locals } in
            interp_block n_env block k
          | _ -> assert false
      )
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
  | Table items ->
    interp_args env items (
      fun (env : env)
          (key, value : exp * exp)
          (k' : (Value.tkey * value) -> unit) : unit ->
        interp_exp env key (
        fun (key : value) : unit ->
          interp_exp env value (
            fun (value : value) : unit ->
              let key = Value.as_table_key key in
              k' (key, value)
          )
      )
    )
    (
      fun (items : (Value.tkey * value) list) : unit ->
        k @@ Table (Hashtbl.of_seq @@ List.to_seq items)
    )

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
