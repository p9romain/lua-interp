(* { locals = [x; y; z; ...]; body; ret } est un block de code de la forme:

   local x, y, z, ...
   body
   return ret
*)
type block = {
  locals: name list;
  body: stat;
  ret: exp
}

(* statement: instruction (tenant sur une ligne) ou séquence d'instructions. *)
and stat =
  (* le statement vide qui ne fait rien *)
  | Nop
  (* séquence de deux statements *)
  | Seq              of stat * stat
  (* Assignation: "x = e" ou "e[e] = e" *)
  | Assign           of var * exp
  (* Appel de fonction: "e(e, ..., e)" *)
  | FunctionCall     of functioncall
  (* Boucle while: "while e do ... end" *)
  | WhileDoEnd       of exp * stat
  (* If-then-else: "if e then ... else ... end" *)
  | If               of exp * stat * stat

and funcbody = name list * block

and name = string

(* expression: s'évalue en une valeur. *)
and exp =
  | Nil
  | False
  | True
  | Integer        of Int64.t
  | Float          of float
  | LiteralString  of string (* chaîne de caractères: de la forme "abc" *)
  | Var            of var (* nom de variable *)
  | FunctionCallE  of functioncall (* appel de fonction "e(e, ..., e)" *)
  | FunctionDef    of funcbody (* fonction anonyme: function (x, y, ...) ... end *)
  | BinOp          of binop * exp * exp (* e op e *)
  | UnOp           of unop * exp (* - e *)
  | Table          of (exp * exp) list
  (* constante représentant une table:
     {}, ou
     { x = e; y = e; ... }, ou
     { [e] = e; [e] = e; ... }
  *)

and functioncall = exp * args

and var =
  | Name       of name (* nom de variable *)
  | IndexTable of exp * exp (* index dans une table: e[e] *)

and args = exp list

and binop =
  (* arithmetic operators *)
  | Addition (* + *)
  | Subtraction (* - *)
  | Multiplication (* * *)
  (* relational operators *)
  | Equality (* == *)
  | Inequality (* ~= *)
  | Less (* < *)
  | Greater (* > *)
  | LessEq (* <= *)
  | GreaterEq (* >= *)
  (* logical operators *)
  | LogicalAnd (* and *)
  | LogicalOr (* or *)

and unop =
  | UnaryMinus (* - *)
  | Not        (* not *)

[@@deriving show]
