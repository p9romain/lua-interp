open Luaparser.Ast

(* Type des clefs dans les tables : entiers ou chaînes de caractères *)
type tkey =
  | KInt of Int64.t
  | KString of string

(* Type des valeurs mini-Lua *)
type t =
  | Nil
  | Bool of bool
  | Int of Int64.t
  | Float of float
  | String of string
  | Function of func
  | Coroutine of coroutine
  | Table of (tkey, t) Hashtbl.t

and func =
  (* fermeture: noms des arguments, environnement et code *)
  | Closure of name list * env * block
  | Print (* la fonction primitive 'print' *)
  | CoroutCreate (* la primitive coroutine.create *)
  | CoroutResume (* la primitive coroutine.mini_resume *)
  | CoroutYield (* la primitive coroutine.yield *)
  | CoroutStatus (* la primitive coroutine.status *)

(* Environnement de l'interpréteur *)
and env = {
  globals : (name, t) Hashtbl.t;
  locals : (name, t) Hashtbl.t list;
}

(* La représentation d'une coroutine *)
and coroutine = {
  mutable stat : coroutine_status
}

(* L'état interne d'une coroutine *)
and coroutine_status =
  | Dead
  | Running of (t -> unit)
  | Suspended of (t -> unit)

(* Convertit une valeur en chaîne pour l'affichage *)
val to_string : t -> string

(* Convertit une valeur en nombre flottant si possible, et renvoie une erreur
   sinon. *)
val as_float : t -> float

(* Convertit une valeur en booléen si possible, et renvoie une erreur sinon. *)
val as_bool : t -> bool

(* Convertit une valeur en fonction si possible, et renvoie une erreur sinon. *)
val as_function : t -> func

(* Convertit une valeur en coroutine si possible, et renvoie une erreur
   sinon. *)
val as_coroutine : t -> coroutine

(* Convertit une valeur en clef servant à indicer une table si possible, et
   renvoie une erreur sinon. *)
val as_table_key : t -> tkey

(* Convertit une valeur en table si possible, et renvoie une erreur sinon. *)
val as_table : t -> (tkey, t) Hashtbl.t

(* Opération d'addition. *)
val add : t -> t -> t

(* Opération de soustraction. *)
val sub : t -> t -> t

(* Opération de négation. *)
val neg : t -> t

(* Opération de multiplication. *)
val mul : t -> t -> t

(* Opération d'égalité. *)
val equal : t -> t -> bool

(* Opération de comparaison (plus petit que). *)
val lt : t -> t -> bool

(* Opération de comparaison (plus petit ou égal que). *)
val le : t -> t -> bool

(* [lookup_ident env id] renvoie la valeur correspondant au nom de variable [id]
   dans l'environnement [env]. *)
val lookup_ident : env -> name -> t

(* [set_ident env id v] associe associe la valeur [v] au nom de variable [id]
   dans l'environnement [env]. *)
val set_ident : env -> name -> t -> unit
