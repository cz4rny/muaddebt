(* TODO: from ripgrep.ml *)
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

let parse_rg_location (line : string) : int * Todo.location =
  try
    let i_null = String.index line '\000' in
    let i_line_end = String.index_from line (i_null + 1) ':' in
    let i_col_end = String.index_from line (i_line_end + 1) ':' in

    let result : Todo.location =
      {
        file = String.sub line 0 i_null;
        line =
          int_of_string (String.sub line (i_null + 1) (i_line_end - i_null - 1));
        column =
          int_of_string
            (String.sub line (i_line_end + 1) (i_col_end - i_line_end - 1));
      }
    in
    let read = i_col_end in
    (read, result)
  with
  | Not_found -> failwith ("Malformed output (missing delimiters): " ^ line)
  | Failure _ -> failwith ("Invalid integer in line/col: " ^ line)

let match_at (str : string) (pos : int) (prefix : string) : (int * int) option =
  let len_str = String.length str in
  let len_pre = String.length prefix in
  if len_str - pos < len_pre then None
  else
    let urgency_char = prefix.[len_pre - 1] in

    let rec loop (i : int) (urgency : int) =
      let current_abs_pos = pos + i in

      if current_abs_pos >= len_str then
        if i >= len_pre then Some (pos + i, urgency) else None
      else if i >= len_pre then
        if str.[pos + i] <> urgency_char then Some (pos + i, urgency)
        else loop (i + 1) (urgency + 1)
      else if str.[pos + i] <> prefix.[i] then None
      else loop (i + 1) 0
    in
    loop 0 0

let parse_rg_line (line : string) : Todo.t option =
  let read, location = parse_rg_location line in

  let total_len = String.length line in
  let rec scan current_idx : Todo.t option =
    if current_idx >= total_len then (
      Printf.eprintf "No marker found in: %s\n" line;
      None)
    else
      let try_match (marker_str, marker) =
        match match_at line current_idx marker_str with
        | None -> None
        | Some (idx_read, urgency) -> Some (idx_read, marker, urgency)
      in
      match List.find_map try_match Marker.all_strings with
      | None -> scan (current_idx + 1)
      | Some (idx_read, marker, urgency) -> (
          match String.index_from_opt line idx_read ' ' with
          | None ->
              Printf.eprintf "No message on line: %s" line;
              None
          | Some idx_space ->
              let msg =
                String.trim (String.sub line idx_space (total_len - idx_space))
              in
              Some ({ location; marker; urgency; msg } : Todo.t))
  in

  scan (read + 1)

let parse_rg_output (lines : string list) : Todo.t list =
  List.filter_map parse_rg_line lines

let find_todos (include_readme : bool) : (Todo.t list, process_error) result =
  let args =
    Array.concat
      [
        rg_command;
        (if include_readme then [||] else [| ignore_readme_glob |]);
        [| markers_regex |];
      ]
  in
  run_rg args |> Result.map parse_rg_output
