%{
  open Internalast
%}

%token EQUAL
%token COLON
%token SEMICOLON
%token COMMA
%token DOT
%token EOF
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET

%token PLUS
%token HYPHEN
%token ASTERISK
%token DOUBLESLASH
%token SLASH
%token HAT
%token PERCENT
%token AMPERSAND
%token TILDA
%token VERTICALBAR
%token DOUBLECOLON
%token DOUBLELT
%token DOUBLEGT
%token DOUBLEDOT
%token TRIPLEDOT
%token GT
%token GTEQ
%token LT
%token LTEQ
%token DOUBLEEQUAL
%token TILDAEQUAL
%token SHARP

(* reserved keywords *)
%token AND
%token BREAK
%token DO
%token ELSE
%token ELSEIF
%token END
%token FALSE
%token FOR
%token FUNCTION
%token GOTO
%token IF
%token IN
%token LOCAL
%token NIL
%token NOT
%token OR
%token REPEAT
%token RETURN
%token THEN
%token TRUE
%token UNTIL
%token WHILE

%token <Int64.t> INTEGER
%token <float> FLOAT
%token <Internalast.name> NAME
%token <string> LITERALSTRING
%start <Internalast.block> chunk
%%

chunk:
  | b = block; EOF { b }
  ;

block:
  | r = retstat? { [], r }
  | SEMICOLON; b = block { b }
  | s = stat; b = block { s::fst b, snd b }
  ;

stat:
  | vs = varlist; EQUAL; es = explist { Assign (vs, es) }
  | fc = functioncall { FunctionCall fc }
  | l = label { l }
  | BREAK { Break }
  | GOTO; n = NAME { Goto n }
  | DO; b = block; END { DoEnd b }
  | WHILE; e = exp; DO; b = block; END { WhileDoEnd (e, b) }
  | REPEAT; b = block; UNTIL; e = exp { RepeatUntil (b, e) }
  | IF; e = exp; THEN; b1 = block;
    br = list(ELSEIF; e = exp; THEN; b = block { (e, b) });
    b2 = option(ELSE; b = block { b }); END
      { If ((e, b1) :: br, b2) }
  | FOR; n = NAME; EQUAL;
    e1 = exp; COMMA; e2 = exp; e3 = option(COMMA; e = exp { e });
    DO; b = block; END
      { ForStep (n, e1, e2, e3, b) }
  | FOR; ns = namelist; IN; es = explist;
    DO; b = block; END
      { ForIn (ns, es, b) }
  | FUNCTION; n = separated_nonempty_list(DOT, NAME);
    _l = option(methodcall); fb = funcbody
    { let fb =
        let (params, vararg, body) = fb
        in FunctionDef ("self" :: params, vararg, body)
      in
      (* "a.b.c" -> IndexTable ( ... , LiteralString c) *)
      let rec var_of_names : name list -> var = function
        | [] -> failwith "var_of_names"
        | [n] -> Name n
        | n :: ns -> IndexTable (Var (var_of_names ns), LiteralString n)
      in
      let v = var_of_names (List.rev n) in
      Assign ([v], [fb]) }
  | LOCAL;
    FUNCTION; n = NAME; _l = option(COLON; n = NAME { n }); fb = funcbody
    { LocalAssign ([n], Some [FunctionDef fb]) }
  | LOCAL; ns = namelist; es = option(EQUAL; es = explist { es })
    { LocalAssign (ns, es) }
  ;

methodcall:
  | COLON; n = NAME { n }
  ;

retstat:
  | RETURN; es = explist; SEMICOLON? { es }
  | RETURN; SEMICOLON? { [] }
  ;

label:
  | DOUBLECOLON; n = NAME; DOUBLECOLON { Label n }
  ;

var:
  | n = NAME { Name n }
  | pe = prefixexp; LBRACKET; e = exp; RBRACKET { IndexTable (pe, e) }
  | pe = prefixexp; DOT; n = NAME { IndexTable (pe, LiteralString n) }
  ;

exp:
  | e1 = exp; OR; e2 = exp1 { BinOp (LogicalOr, e1, e2) }
  | e = exp1 { e }
  ;

exp1:
  | e1 = exp1; AND; e2 = exp2 { BinOp (LogicalAnd, e1, e2) }
  | e = exp2 { e }
  ;

exp2:
  | e1 = exp2; b = binop2; e2 = exp3 { BinOp (b, e1, e2) }
  | e = exp3 { e }
  ;

exp3:
  | e1 = exp3; VERTICALBAR; e2 = exp4 { BinOp (BitwiseOr, e1, e2) }
  | e = exp4 { e }
  ;

exp4:
  | e1 = exp4; TILDA; e2 = exp5 { BinOp (BitwiseXor, e1, e2) }
  | e = exp5 { e }
  ;

exp5:
  | e1 = exp5; AMPERSAND; e2 = exp6 { BinOp (BitwiseAnd, e1, e2) }
  | e = exp6 { e }
  ;

exp6:
  | e1 = exp6; b = binop6; e2 = exp7 { BinOp (b, e1, e2) }
  | e = exp7 { e }
  ;

exp7:
  | e1 = exp8; DOUBLEDOT; e2 = exp7 { BinOp (Concat, e1, e2) }
  | e = exp8 { e }
  ;

exp8:
  | e1 = exp8; b = binop8; e2 = exp9 { BinOp (b, e1, e2) }
  | e = exp9 { e }
  ;

exp9:
  | e1 = exp9; b = binop9; e2 = exp10 { BinOp (b, e1, e2) }
  | e = exp10 { e }
  ;

exp10:
  | u = unop; e = exp11 { UnOp (u, e) }
  | e = exp11 { e }
  ;

exp11:
  | e1 = base_exp; HAT; e2 = exp11 { BinOp (Exponentiation, e1, e2) }
  | e = base_exp { e }
  ;

base_exp:
  | NIL { Nil }
  | TRUE { True }
  | FALSE { False }
  | p = prefixexp { p }
  | i = INTEGER { Integer i }
  | f = FLOAT { Float f }
  | s = LITERALSTRING { LiteralString s }
  | t = tableconstr { Table t }
  | TRIPLEDOT { Vararg }
  | FUNCTION; fb = funcbody { FunctionDef fb }
  ;

(* Made inline so that conflicts are shift/reduce conflicts and are hence
   resolved by shifting (which is what we want). *)
%inline prefixexp:
  | fc = functioncall { FunctionCallE fc }
  | p = pexp { p }
  ;

namelist:
  | ns = separated_nonempty_list(COMMA, NAME) { ns }
  ;

varlist:
  | vs = separated_nonempty_list(COMMA, var) { vs }
  ;

explist:
  | es = separated_nonempty_list(COMMA, exp) { es }
  ;

tableconstr:
  | LBRACE; RBRACE { [] }
  | LBRACE; fs = fields; RBRACE { fs }
  ;

field_separator:
  | COMMA     { () }
  | SEMICOLON { () }
  ;

fields:
  | f = field { [f] }
  | f = field; field_separator; fs = fields?
    { match fs with
      | Some fs -> f :: fs
      | None    -> [f]
    }
  ;

field:
  | LBRACKET; k = exp; RBRACKET; EQUAL; v = exp { Some k, v }
  | n = NAME; EQUAL; v = exp { Some (LiteralString n), v }
  | v = exp { None, v }
  ;

%inline binop2:
  | LT { Less }
  | GT { Greater }
  | LTEQ { LessEq }
  | GTEQ { GreaterEq }
  | TILDAEQUAL { Inequality }
  | DOUBLEEQUAL { Equality }
  ;

%inline binop6:
  | DOUBLELT { ShiftLeft }
  | DOUBLEGT { ShiftRight }
  ;

%inline binop8:
  | PLUS { Addition }
  | HYPHEN { Subtraction }
  ;

%inline binop9:
  | ASTERISK { Multiplication }
  | SLASH { FloatDivision }
  | DOUBLESLASH { FloorDivision }
  | PERCENT { Modulo }
  ;

%inline unop:
  | HYPHEN { UnaryMinus }
  | NOT { LogicalNot }
  | SHARP { Length }
  | TILDA { BitwiseNot }
  ;

pexp:
  | v = var { Var v }
  | LPAREN; e = exp; RPAREN { e }
  ;

functioncall:
  | p = prefixexp; a = args { Function (p, a) }
  | p = prefixexp; COLON; n = NAME; a = args { Method(p, n, a) }
  ;

funcbody:
  | LPAREN; pl = option(parlist); RPAREN; b = block; END
    { let ps, va = Option.value pl ~default:([], None) in
      ps, va, b }
  ;

parlist:
  | TRIPLEDOT { [], Some () }
  | n = NAME { [n], None }
  | n = NAME; COMMA; ps = parlist
    { match ps with ps, va -> n :: ps, va }
  ;

args:
  | LPAREN; RPAREN { [] }
  | LPAREN; es = explist; RPAREN { es }
  | s = LITERALSTRING { [LiteralString s] }
  | t = tableconstr { [Table t] }
  ;
