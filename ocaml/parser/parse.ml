open Lexing

exception Parse_error of string * int * int

let of_lexbuf lexbuf =
  try Simplify.simp_outerblock (Parser.chunk Lexer.read lexbuf) with
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    raise (Parse_error (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1))

let of_file filename =
  let cin = open_in filename in
  Fun.protect ~finally:(fun () -> close_in cin) (fun () ->
    of_lexbuf (Lexing.from_channel cin)
  )
