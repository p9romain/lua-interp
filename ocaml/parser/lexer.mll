{
open Lexing
open Parser

let reserved_keywords =
  [ "and",      Parser.AND
  ; "break",    Parser.BREAK
  ; "do",       Parser.DO
  ; "else",     Parser.ELSE
  ; "elseif",   Parser.ELSEIF
  ; "end",      Parser.END
  ; "false",    Parser.FALSE
  ; "for",      Parser.FOR
  ; "function", Parser.FUNCTION
  ; "goto",     Parser.GOTO
  ; "if",       Parser.IF
  ; "in",       Parser.IN
  ; "local",    Parser.LOCAL
  ; "nil",      Parser.NIL
  ; "not",      Parser.NOT
  ; "or",       Parser.OR
  ; "repeat",   Parser.REPEAT
  ; "return",   Parser.RETURN
  ; "then",     Parser.THEN
  ; "true",     Parser.TRUE
  ; "until",    Parser.UNTIL
  ; "while",    Parser.WHILE
  ]

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n" | "\r\n"
let name = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let hex = '0' ['x' 'X']

rule read = parse
  | "--" ('[' '='* '[') as delimiter
      { comment_long (String.length delimiter) lexbuf }
  | "--" { comment lexbuf }
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | name as name
      { try List.assoc name reserved_keywords
        with Not_found -> Parser.NAME name }
  | ['0'-'9']+ '.' ['0'-'9']* (['e' 'E'] ['0'-'9']+)?
  |            '.' ['0'-'9']+ (['e' 'E'] ['0'-'9']+)?
  | ['0'-'9']+                (['e' 'E'] ['0'-'9']+)?
  (* TODO 0x...p... *)
      { try Parser.INTEGER (Int64.of_string (lexeme lexbuf))
        with Failure _ -> Parser.FLOAT (float_of_string (lexeme lexbuf)) }
  | hex ['0'-'9' 'A'-'F' 'a'-'f']+
      { Parser.INTEGER (Int64.of_string (lexeme lexbuf)) }
  | ('\'' | '"') as delimiter
      { Parser.LITERALSTRING
          (literal_string delimiter (Buffer.create 80) lexbuf) }
  | ('[' '='* '[') as delimiter newline?
      { Parser.LITERALSTRING
          (literal_string_long (String.length delimiter)
                               (Buffer.create 80) lexbuf) }
  | '=' { EQUAL }
  | "::" { DOUBLECOLON }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | "..." { TRIPLEDOT }
  | ".." { DOUBLEDOT }
  | '.' { DOT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { HYPHEN }
  | '*' { ASTERISK }
  | "//"{ DOUBLESLASH }
  | '/' { SLASH }
  | '^' { HAT }
  | '%' { PERCENT }
  | '&' { AMPERSAND }
  | '~' { TILDA }
  | '|' { VERTICALBAR }
  | "<<" { DOUBLELT }
  | ">>" { DOUBLEGT }
  | '<' { LT }
  | "<=" { LTEQ }
  | '>' { GT }
  | ">=" { GTEQ }
  | "==" { DOUBLEEQUAL }
  | "~=" { TILDAEQUAL }
  | '#' { SHARP }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | eof { EOF }

and comment_long len = parse
  | ']' { comment_long_brk len lexbuf }
  | newline
      { next_line lexbuf;
        comment_long len lexbuf }
  | [^ '\r' '\n' ']']+
      { comment_long len lexbuf }

and comment_long_brk len = parse
  | '='* as s ']'
      { if String.length s + 2 = len then read lexbuf
        else comment_long_brk len lexbuf }
  | ""
      { comment_long len lexbuf }

and comment = parse
  | [^ '\r' '\n']* (newline | eof) { next_line lexbuf; read lexbuf }

and literal_string open_delimiter buf = parse
  | "\\a"
      { Buffer.add_char buf '\007'; literal_string open_delimiter buf lexbuf }
  | "\\b"
      { Buffer.add_char buf '\008'; literal_string open_delimiter buf lexbuf }
  | "\\f"
      { Buffer.add_char buf '\012'; literal_string open_delimiter buf lexbuf }
  | "\\n"
      { Buffer.add_char buf '\n';   literal_string open_delimiter buf lexbuf }
  | '\\' newline
      { next_line lexbuf;
        Buffer.add_char buf '\n';   literal_string open_delimiter buf lexbuf }
  | "\\r"
      { Buffer.add_char buf '\r';   literal_string open_delimiter buf lexbuf }
  | "\\t"
      { Buffer.add_char buf '\t';   literal_string open_delimiter buf lexbuf }
  | "\\v"
      { Buffer.add_char buf '\011'; literal_string open_delimiter buf lexbuf }
  | "\\\\"
      { Buffer.add_char buf '\\';   literal_string open_delimiter buf lexbuf }
  | "\\\""
      { Buffer.add_char buf '"';    literal_string open_delimiter buf lexbuf }
  | "\\\'"
      { Buffer.add_char buf '\'';   literal_string open_delimiter buf lexbuf }
  | "\\["
      { Buffer.add_char buf '[';    literal_string open_delimiter buf lexbuf }
  | "\\]"
      { Buffer.add_char buf ']';    literal_string open_delimiter buf lexbuf }
  (* \x \u \z TODO *)
  | ['\'' '"'] as c
      { if c = open_delimiter then Buffer.contents buf
        else begin
          Buffer.add_char buf c;
          literal_string open_delimiter buf lexbuf
        end }
  | [^'\\' '\n' '\r' '\'' '"']+ as s
      { Buffer.add_string buf s;
        literal_string open_delimiter buf lexbuf }

and literal_string_long len buf = parse
  | ']' { literal_string_long_brk len buf lexbuf }
  | newline
      { next_line lexbuf;
        Buffer.add_char buf '\n';
        literal_string_long len buf lexbuf }
  | [^ '\r' '\n' ']']+ as s
      { Buffer.add_string buf s;
        literal_string_long len buf lexbuf }

and literal_string_long_brk len buf = parse
  | '='* as s ']'
      { if String.length s + 2 = len then Buffer.contents buf
        else begin
          Buffer.add_char buf ']';
          Buffer.add_string buf s;
          literal_string_long_brk len buf lexbuf
        end }
  | ""
      { Buffer.add_char buf ']';
        literal_string_long len buf lexbuf }
