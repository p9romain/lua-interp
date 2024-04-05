open Luaparser.Ast

type tkey =
  | KInt of Int64.t
  | KString of string

type t =
  | Nil
  | Bool of bool
  | Int of Int64.t
  | Float of float
  | String of string
  | Function of func
  | Table of (tkey, t) Hashtbl.t

and func =
  | Closure of name list * env * block
  | Print

and env = {
  globals : (name, t) Hashtbl.t;
  locals : (name, t) Hashtbl.t list;
}

let to_string = function
  | Nil -> "nil"
  | Bool b -> string_of_bool b
  | Int n -> Int64.to_string n
  | Float f ->
    let b = Buffer.create 15 in
    Printf.bprintf b "%.14g" f;
    if String.for_all (fun c -> c = '-' || '0' <= c && c <= '9') (Buffer.contents b) then
      Buffer.add_string b ".0";
    Buffer.contents b
  | String s -> s
  | Function _ -> "<function>"
  | Table _ -> "<table>"

let as_float = function
  | Float f -> f
  | Int n -> Int64.to_float n
  | v -> failwith (to_string v ^ ": not a float")

let as_bool = function
  | Bool b -> b
  | Nil -> false
  | _ -> true

let as_function = function
  | Function f -> f
  | v -> failwith (to_string v ^ ": not a function")

let as_table_key = function
  | Int n -> KInt n
  | String s -> KString s
  | v -> failwith (to_string v ^ ": not a table key (int or string)")

let as_table = function
  | Table t -> t
  | v -> failwith (to_string v ^ ": not a table")

let add v1 v2 =
  match v1, v2 with
  | Int(n1), Int(n2) -> Int(Int64.add n1 n2)
  | _, _ -> Float(as_float v1 +. as_float v2)

let sub v1 v2 =
  match v1, v2 with
  | Int(n1), Int(n2) -> Int(Int64.sub n1 n2)
  | _, _ -> Float(as_float v1 -. as_float v2)

let neg = function
  | Int(n) -> Int(Int64.neg n)
  | Float(f) -> Float(-.f)
  | v -> failwith (to_string v ^ ": not a number")

let mul v1 v2 =
  match v1, v2 with
  | Int(n1), Int(n2) -> Int(Int64.mul n1 n2)
  | _, _ -> Float(as_float v1 *. as_float v2)

let cmp_int_float n f =
  if Float.is_nan f then None
  else
    let c = compare (Int64.to_float n) f in
    if c < 0 then Some(-1)
    else if c > 0 then Some(1)
    else if n < Int64.of_float (ceil f) then Some(-1)
    else if n > Int64.of_float (floor f) then Some(1)
    else Some(0)

let equal v1 v2 =
  v1 == v2 ||
  match v1, v2 with
  | Nil, Nil -> true
  | Bool(b1), Bool(b2) -> b1 = b2
  | Int(n1), Int(n2) -> n1 = n2
  | Float(f), Int(n) | Int(n), Float(f) ->
    cmp_int_float n f = Some(0)
  | Float(f1), Float(f2) -> f1 = f2
  | String(s1), String(s2) -> s1 = s2
  | _, _ -> false

let lt v1 v2 =
  match v1, v2 with
  | Int(n1), Int(n2) -> n1 < n2
  | Float(f1), Int(n2) ->
    cmp_int_float n2 f1 = Some(1)
  | Int(n1), Float(f2) ->
    cmp_int_float n1 f2 = Some(-1)
  | Float(f1), Float(f2) -> f1 < f2
  | _, _ ->
    failwith ("trying to compare " ^ to_string v1 ^ " and " ^ to_string v2)

let le v1 v2 =
  match v1, v2 with
  | Int(n1), Int(n2) -> n1 <= n2
  | Float(f1), Int(n2) ->
    begin match cmp_int_float n2 f1 with Some(0|1) -> true | _ -> false end
  | Int(n1), Float(f2) ->
    begin match cmp_int_float n1 f2 with Some(0|(-1)) -> true | _ -> false end
  | Float(f1), Float(f2) -> f1 <= f2
  | _, _ ->
    failwith ("trying to compare " ^ to_string v1 ^ " and " ^ to_string v2)

let lookup_scope env name =
  try
    List.find (fun local_scope ->
      Hashtbl.mem local_scope name
    ) env.locals
  with Not_found ->
    env.globals

let lookup_ident env name =
  try Hashtbl.find (lookup_scope env name) name with
    Not_found -> Nil

let set_ident env name v =
  Hashtbl.replace (lookup_scope env name) name v
