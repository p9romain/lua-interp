type input =
  | Dir of string
  | File of string

let test_input =
  match Sys.argv |> Array.to_list |> List.tl with
  | [path] ->
    if not (Sys.file_exists path) then (
      Printf.eprintf "%s: not found\n" path; exit 1
    );
    if Sys.is_directory path then Dir path
    else File path
  | _ -> Format.eprintf "usage: %s <tests_directory>\n" Sys.argv.(0); exit 1

let read_all_gen read cin =
  let buf = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let rec loop () =
    match read cin b 0 4096 with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf b 0 n; loop ()
  in
  loop ();
  Buffer.contents buf

let read_all_channel = read_all_gen input

let run_exec_on_file exec file =
  let stdout, stdin, stderr = Unix.open_process_args_full
      exec [|exec; file|] (Unix.environment ())
  in
  let output = read_all_channel stdout in
  let err = read_all_channel stderr in
  let ret = Unix.close_process_full (stdout, stdin, stderr) = WEXITED(0) in
  output, err, ret

type runner = string -> string * string * bool

let run_lua_on_file : runner = fun file ->
  run_exec_on_file "lua/mini_lua.sh" file

let run_ocaml_interp_on_file : runner = fun file ->
  run_exec_on_file "ocaml/_build/default/interp/run.exe" file

let run_ocaml_cps_interp_on_file : runner = fun file ->
  run_exec_on_file "ocaml/_build/default/interp-cps/run.exe" file

let run_rust_interp_on_file : runner = fun file ->
  run_exec_on_file "rust/target/release/lua" file

type test_output = {
  output : string; err : string; success : bool;
  reference_output : string; reference_err : string;
  reference_success : bool;
}

let green s = "\027[1;32m" ^ s ^ "\027[0m"
let red s = "\027[1;31m" ^ s ^ "\027[0m"

let ensure_newline = function
  | "" -> ""
  | s ->
    if s.[String.length s - 1] = '\n' then s
    else s ^ "\n"

let runtest (runner: runner) (file: string) : (string * bool, test_output) result =
  let reference_output, reference_err, reference_success = run_lua_on_file file in
  let output, err, success = runner file in

  if output = reference_output && success = reference_success
  then Ok (output, success)
  else Error ({
    reference_output; reference_err; success;
    output; err; reference_success
  })

let test_short runners file =
  Printf.printf "Testing %s...%!" file;
  List.iter (fun (runner, runner_name) ->
    match runtest runner file with
    | Ok (_output, success) ->
      Printf.printf " %s%!"
        (green (Printf.sprintf "[%s: OK%s]" runner_name (if success then "" else " (failure)")))
    | Error _ ->
      Printf.printf " %s%!"
        (red (Printf.sprintf "[%s: ERROR]" runner_name))
  ) runners;
  Printf.printf "\n%!"

let test_long runner file =
  Printf.printf "Testing %s... %!" file;
  match runtest runner file with
  | Ok (_output, success) ->
    Printf.printf "%s %s\n%!" (green "[OK]")
      (if success then "" else "(failure)");

  | Error ({ output; err; success;
             reference_output; reference_err; reference_success }) ->
    Printf.printf "%s\n%!" (red "[ERROR]");
    Printf.printf "%s " (red "==>");
    (match success, reference_success with
     | true, true ->
       Printf.printf "Mismatch between lua and the interpreter.\n"
     | true, false ->
       Printf.printf "Error while running the reference interpreter.\n"
     | false, true ->
       Printf.printf "Error while running the interpreter.\n"
     | false, false ->
       Printf.printf "Errors while running the reference interpreter and the program.\n"
    );
    Printf.printf "- Running 'lua' on the program produces:\n";

    Printf.printf "%s%s\n"
      (ensure_newline reference_output) (ensure_newline reference_err)
    ;
    Printf.printf "- Running the interpreter on the program produces:\n";
    Printf.printf "%s%s\n"
      (ensure_newline output) (ensure_newline err);
    if output <> reference_output && String.trim output = String.trim reference_output then (
      Printf.printf "%s: The reference and interpreter output only differ with respect to whitespace.\n\
                    \      Maybe check for extra \\n or spaces.\n"
        (green "Hint")
    )

let runners_no_co : (runner * string) list =
  [(run_ocaml_interp_on_file, "ocaml");
   (run_rust_interp_on_file, "rust");
   (run_ocaml_cps_interp_on_file, "ocaml+cps")]

let runners_co : (runner * string) list =
  [(run_ocaml_cps_interp_on_file, "ocaml+cps")]

let test_dir tests_dir =
  let testfiles =
    Sys.readdir tests_dir
    |> Array.to_list
    |> List.filter (fun file -> Filename.extension file = ".lua")
    |> List.sort String.compare
    |> List.map (fun file -> Filename.concat tests_dir file)
  in
  let testfiles_co =
    List.filter (fun f -> Filename.check_suffix f ".co.lua") testfiles in
  let testfiles_no_co =
    List.filter (fun f -> not (Filename.check_suffix f ".co.lua")) testfiles in
  List.iter (fun file ->
    test_short runners_no_co file
  ) testfiles_no_co;
  List.iter (fun file ->
    test_short runners_co file
  ) testfiles_co

let test_file test_file =
  Printf.printf "============== Running interpreter written in OCaml  ==============\n";
  test_long run_ocaml_interp_on_file test_file;
  Printf.printf "\n============== Running interpreter written in Rust ==============\n";
  test_long run_rust_interp_on_file test_file;
  if Filename.check_suffix test_file ".co.lua" then (
    Printf.printf "\n============== Running interpreter written in OCaml (CPS variant) ==============\n";
    test_long run_ocaml_cps_interp_on_file test_file
  )

let () =
  match test_input with
  | Dir dir -> test_dir dir
  | File file -> test_file file
