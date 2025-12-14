type process_error =
  | Launch_failed of string
  (* | Read_failed of string *)
  | Execution_failed of int

let rg_command =
  [| "rg"; "--pcre2"; "--null"; "--no-heading"; "--line-number"; "--column" |]

(* Attempt 
 * (?m) — multi-line mode
 * \s*  — possible whitespace
 * \b   — end of word bounds
 * //   — C, C++, Java, C#, JS/TS, Rust, Go Swift, Kotlin, other C-like langs
 * #    — shell scripts, Python, Ruby, Perl, Makefiles, YAML, TOML,
 *        config formats: Dockerfile, .env, etc.
 * --   — Haskell, SQLs, Lua, Ada, some assemblies
 * ;    — LISP langs, assemblies, old-style config files
 * %    — LaTeX, PostScript, math DSLs
 * \(\* — ML functional langs, like OCaml, F#
 * /\*  — Same as //, but multi-line
 * \*   – Same as //, but multi-line
 * <!-- — markup langs, like HTML, XML, JSX/TSX
 *)
let markers_regex =
  {|(?m)^\s*(//|#|--|;|%|\(\*|/\*|\*|<!--)\s*(TODO+|FIX+|FIXME+|HACK+|BUG+)\b|}

let ignore_readme_glob = "--glob=!README.md"

let run_rg (args : string array) : (string list, process_error) result =
  let cmd = args.(0) in
  match Unix.open_process_args_in cmd args with
  | exception Unix.Unix_error (err, _, _) ->
      Error (Launch_failed (Unix.error_message err))
  | channel -> (
      match In_channel.input_lines channel with
      | exception exn ->
          ignore (Unix.close_process_in channel);
          raise exn
      | output -> (
          match Unix.close_process_in channel with
          | Unix.WEXITED 0 ->
              let out_lines = String.concat "\n" output in
              Printf.eprintf "Ripgrep output:\n%s\n" out_lines;
              Ok output
          | Unix.WEXITED 1 -> Ok []
          | Unix.WEXITED code -> Error (Execution_failed code)
          | _ -> Error (Execution_failed (-1))))

let find_todos (include_readme : bool) : (string list, process_error) result =
  let args =
    Array.concat
      [
        rg_command;
        (if include_readme then [||] else [| ignore_readme_glob |]);
        [| markers_regex |];
      ]
  in
  run_rg args
