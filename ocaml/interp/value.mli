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
  | Table of (tkey, t) Hashtbl.t

and func =
  (* fermeture: noms des arguments, environnement et code *)
  | Closure of name list * env * block
  | Print (* la fonction primitive 'print' *)

(* Environnement de l'interpréteur stockant le contenu des variables. La table
   "globals" stocke le contenu des variables globales; la liste "locals" stocke
   les tables de variables locales.

   Lorsqu'un nom de variable apparaît dans plusieurs tables locales, la table
   apparaissant en premier dans la liste prend le dessus.

   Si un nom de variable apparaît dans une table locale et dans une table
   globale, la table locale prend le dessus.
*)
and env = {
  globals : (name, t) Hashtbl.t;
  locals : (name, t) Hashtbl.t list;
}

(* Convertit une valeur en chaîne pour l'affichage *)
val to_string : t -> string

(* Convertit une valeur en nombre flottant si possible, et renvoie une erreur
   sinon. *)
val as_float : t -> float

(* Convertit une valeur en booléen si possible, et renvoie une erreur sinon. *)
val as_bool : t -> bool

(* Convertit une valeur en fonction si possible, et renvoie une erreur sinon. *)
val as_function : t -> func

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
