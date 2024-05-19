open Luaparser.Ast
type value = Value.t
type coroutine = Value.coroutine
type env = Value.env

let coroutine_error (_ : value) : unit = 
  failwith "[interp_error] we're not in a coroutine"

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

let interp_args (env : env) 
                (l : 'a list) 
                (curr_co : coroutine)
                (f : env -> 'a -> coroutine -> ('b -> unit) -> unit)
                (k : 'b list -> unit) : unit =
    let rec loop (acc : 'b list) 
                 (l : 'a list) 
                 (k : 'b list -> unit) : unit =
      match l with
      | [] -> k @@ List.rev acc
      | expr :: ll ->
        f env expr curr_co (
          fun (res : 'b) : unit ->
            loop (res :: acc) ll k
        )
    in
    loop [] l k

let rec interp_block (env : env) 
                 (blk : block)
                 (curr_co : coroutine)
                 (k : value -> unit) : unit =
  let local_values = create_scope blk.locals [] in
  let n_env = Value.{ env with locals = local_values :: env.locals } in
  interp_stat n_env blk.body curr_co (
    fun (_ : unit) : unit ->
      interp_exp n_env blk.ret curr_co k
  )

and interp_stat (env : env) 
                (stat : stat) 
                (curr_co : coroutine)
                (k : unit -> unit) : unit =
  match stat with
  | Nop -> k ()
  | Seq (stat, stat') ->
    interp_stat env stat curr_co (
      fun (_ : unit) : unit ->
        interp_stat env stat' curr_co k
    )
  | Assign (var, expr) -> (
    match var with
    | Name name ->
      interp_exp env expr curr_co (
        fun (value : value) : unit ->
          Value.set_ident env name value ;
          k ()
      )
    | IndexTable (table, key) ->
      interp_exp env table curr_co (
        fun (table : value) : unit ->
          interp_exp env key curr_co (
            fun (key : value) : unit ->
              interp_exp env expr curr_co (
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
    interp_funcall env func curr_co (
      fun (ret : value) : unit ->
        match ret with
        | Nil -> k ()
        | _ -> failwith 
          @@ Format.sprintf "[interp_error] %s : return value not nil" 
          @@ Value.to_string ret
    )
  )
  | WhileDoEnd (expr, stat) ->
    interp_exp env expr curr_co (
      fun (value : value) : unit ->
        if Value.as_bool value then
          interp_stat env stat curr_co (
            fun (_ : unit) : unit ->
              interp_stat env (WhileDoEnd (expr, stat)) curr_co k
          )
        else
          k ()
    )
  | If (expr, stat_then, stat_else) ->
    interp_exp env expr curr_co (
      fun (value : value) : unit ->
        if Value.as_bool value then
          interp_stat env stat_then curr_co k
        else
          interp_stat env stat_else curr_co k
    )

and interp_funcall (env : env) 
                   (func, values : functioncall)
                   (curr_co : coroutine) 
                   (k: value -> unit) : unit =
  interp_args env values curr_co interp_exp (
    fun (values : value list) : unit ->
      interp_exp env func curr_co (
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
            interp_block n_env block curr_co k
          | CoroutCreate -> (
            match values with
            | co :: _ -> (
              match Value.as_function co with
              | Closure (args, lenv, block) ->
                let new_co = Value.{ stat = Dead } in
                (* when calling for the coroutine *)
                let new_k = (
                  fun (value : value) : unit ->
                    let scope = create_scope args [ value ] in
                    let n_env = Value.{ env with locals = scope :: lenv.locals } in
                    (* after the end of the function *)
                    let new_k' = (
                      fun (value' : value) : unit ->
                        match new_co.stat with
                        | Running old_co -> 
                          (* killing the coroutine (can't be called again) *)
                          new_co.stat <- Dead ;
                          (* keep going what we have to do *)
                          old_co value'
                        | _ -> assert false
                    )
                    in
                    interp_block n_env block new_co new_k'
                ) 
                in
                let () = new_co.stat <- Suspended new_k in
                k @@ Coroutine new_co
              | _ -> failwith "[interp_error] given argument must be a closure"
            )
            | [] -> failwith "[interp_error] needs at least one argument"
          )
          | CoroutResume -> (
            match values with
            | co :: args -> (
              let co = Value.as_coroutine co in
              match co.stat with
              | Suspended k' -> (
                let () = co.stat <- Running k in
                match args with
                | arg :: _ -> k' arg
                | [] -> k' Nil
              )
              | _ -> failwith "[interp_error] coroutine must be suspended"
            )
            | [] -> failwith "[interp_error] needs at least one argument"
          )
          | CoroutYield -> (
            match curr_co.stat with
            | Running k' -> (
              let () = curr_co.stat <- Suspended k in
              match values with
              | arg :: _ -> k' arg
              | [] -> k' Nil
            )
            | _ -> failwith "[interp_error] current coroutine must be running "
          )
          | CoroutStatus -> (
            match values with
            | co :: _ -> (
              match (Value.as_coroutine co).stat with
              | Dead -> k @@ String "dead"
              | Running _ -> k @@ String "running"
              | Suspended _ -> k @@ String "suspended"
            )
            | [] -> k Nil
          )
      )
  )

and interp_exp (env : env) 
               (e : exp) 
               (curr_co : coroutine)
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
      interp_exp env table curr_co (
        fun (table : value) : unit ->
          interp_exp env key curr_co (
            fun (key : value) : unit ->
              let table = Value.as_table table in
              let key = Value.as_table_key key in
              k @@ Option.value (Hashtbl.find_opt table key) ~default:Nil
          )
      )
  )
  | FunctionCallE func -> interp_funcall env func curr_co k
  | FunctionDef (args, block) ->
    k @@ Function(
      Closure(
        args,
        env,
        block
      )
    )
  | BinOp (bop, lhs, rhs) -> (
    interp_exp env lhs curr_co (
      fun (interp_lhs : value) : unit ->
        match bop with
        | LogicalAnd ->
          if Value.as_bool interp_lhs then
            interp_exp env rhs curr_co k
          else
            k @@ interp_lhs
        | LogicalOr ->
          if Value.as_bool interp_lhs then
            k @@ interp_lhs
          else
            interp_exp env rhs curr_co k
        | _ ->
          interp_exp env rhs curr_co (
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
    interp_exp env expr curr_co (
      fun (expr : value) : unit ->
        match uop with
        | UnaryMinus -> k @@ Value.neg expr
        | Not -> k @@ Bool (not @@ Value.as_bool expr)
    )
  )
  | Table items ->
    interp_args env items curr_co (
      fun (env : env)
          (key, value : exp * exp)
          (curr_co' : coroutine)
          (k' : (Value.tkey * value) -> unit) : unit ->
        interp_exp env key curr_co' (
        fun (key : value) : unit ->
          interp_exp env value curr_co' (
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

  interp_block env ast Value.{ stat = Running coroutine_error } (fun _ -> ())