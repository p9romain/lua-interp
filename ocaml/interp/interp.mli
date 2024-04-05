(* Le point d'entrée de l'interprète : prend un arbre de syntaxe mini-Lua en
   entrée et l'exécute. *)
val run : Luaparser.Ast.block -> unit
