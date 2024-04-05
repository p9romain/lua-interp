open Luaparser.Ast
type value = Value.t
type env = Value.env

(* Fonction auxiliaire pour créer une table d'environnement à partir de noms et
   valeurs associées. *)
let create_scope (names: string list) (values: value list) : (name, value) Hashtbl.t =
  assert false

(* Fonctions de l'interprète, mutuellement récursives. Une fonction par
   catégorie syntaxique de l'AST. *)

(* Interprète un bloc de code *)
let rec interp_block (env : env) (blk : block) : value =
  assert false

(* Interprète un statement *)
and interp_stat (env : env) (stat : stat) : unit =
  assert false

(* Interprète un appel de fonction *)
and interp_funcall (env : env) (fc : functioncall) : value =
  assert false

(* Interprète une expression *)
and interp_exp (env : env) (e : exp) : value =
 assert false

let run ast =
  let globals = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Value.Function Print);
  let env = Value.{ globals; locals = [] } in
  ignore (interp_block env ast)
