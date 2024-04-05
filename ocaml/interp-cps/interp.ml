open Luaparser.Ast
type value = Value.t
type coroutine = Value.coroutine
type env = Value.env

let rec interp_block (env : env) (blk : block) (k : value -> unit) : unit =
  assert false

and interp_stat (env : env) (stat : stat) (k : unit -> unit) : unit =
  assert false

and interp_funcall (env : env) (fc : functioncall) (k: value -> unit) : unit =
  assert false

and interp_exp (env : env) (e : exp) (k: value -> unit) : unit =
  assert false

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

  assert false
