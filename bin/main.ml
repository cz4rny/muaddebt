open Muaddebt

let () =
  let include_readme = ref false in
  let specs =
    [
      ( "--include-readme",
        Arg.Set include_readme,
        "Include README.md in the scan" );
    ]
  in
  Arg.parse specs (fun _ -> ()) "Usage: muaddebt [options]";

  print_endline "Scanning for TODOs, FIXes, FIXMEs, BUGs, and HACKs...";

  match Ripgrep.find_todos !include_readme with
  | Error (Ripgrep.Execution_failed code) ->
      Printf.eprintf "Error: rg exited with code %d\n%!" code
  | Error (Ripgrep.Launch_failed msg) ->
      Printf.eprintf "Error: Failed to launch rg: %s\n%!" msg
  | Ok [] -> Printf.eprintf "✓ No issues found!\n%!"
  | Ok todos -> (
      match Readme_io.update_readme todos with
      | Ok () -> ()
      | Error e ->
          Printf.eprintf "Failed to update: %s\n%!" (Readme_io.error_message e))
