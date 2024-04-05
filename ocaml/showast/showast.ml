open Luaparser

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | [filename] ->
    begin match Luaparser.Parse.of_file filename with
      | ast ->
        Format.printf "%a\n@." Ast.pp_block ast
      | exception (Parse.Parse_error (_, n, m)) ->
        Printf.eprintf "Parsing error\n";
        Printf.eprintf "%s:%d:%d: syntax error\n" filename n m;
        exit 1
      | exception (Simplify.Unhandled_ast reason) ->
        Printf.eprintf "Input program uses a lua feature that mini-lua does not support: %s\n"
          reason;
        exit 1
    end
  | _ ->
    Printf.eprintf "Usage: %s <file.lua>\n" Sys.argv.(0);
    exit 1
