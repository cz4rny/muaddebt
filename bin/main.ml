open Result.Syntax
open Muaddebt

let or_die ?(code = 1) fmt =
  fmt |> Printf.ksprintf (fun msg -> function
    | Ok v -> v
    | Error e -> 
        Printf.eprintf "%s: %s\n%!" msg e;
        exit code
  )

let () =
  let include_readme = ref false in
  let specs          = [
      ( "--include-readme", Arg.Set include_readme, "Include README.md in the scan" );
    ] in
  Arg.parse specs (fun _ -> ()) "Usage: muaddebt [options]";

  print_endline "Scanning for TODOs, FIXes, FIXMEs, BUGs, and HACKs...";

  let todos = Ripgrep.find_todos !include_readme
    |> Result.map_error begin function
      | Ripgrep.Execution_failed code -> "rg exited with code: "  ^ (string_of_int code)
      | Ripgrep.Launch_failed msg     -> "Failed to launch rg: "  ^ msg
    end |> or_die ""
  in

  match todos with
    | [] -> Printf.eprintf "👏 No tech-debt found, good job!\n%!";
    | todos -> Readme_io.update_readme todos 
        |> Result.map_error Readme_io.error_message
        |> or_die "Failed to update %s" Readme_io.default_file_path
