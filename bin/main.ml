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
      Printf.eprintf "Error: rg exited with code %d\n" code
  | Error (Ripgrep.Launch_failed msg) ->
      Printf.eprintf "Error: Failed to launch rg: %s\n" msg
  | Ok [] -> Printf.eprintf "✓ No issues found!\n"
  | Ok todos -> (
      let open Readme_io in
      let open Result.Syntax in
      let result =
        let buff = Buffer.create 1024 in
        let fmt = Format.formatter_of_buffer buff in
        let* lines = read () in

        let rec copy_until_dashboard (start_lines : string list) : string list =
          match start_lines with
          | [] -> []
          | l :: rest ->
              if Dash_md.is_header l then rest
              else begin
                Format.fprintf fmt "%s@." l;
                copy_until_dashboard rest
              end
        in
        let after_start = copy_until_dashboard lines in

        let rec drop_while_dashboard (drop_lines : string list) : string list =
          match drop_lines with
          | [] -> []
          | l :: rest ->
              if Dash_md.is_footer l then rest else drop_while_dashboard rest
        in
        let after_dashboard = drop_while_dashboard after_start in

        if after_dashboard = [] then Format.fprintf fmt "@.";
        Dash_md.pp_dashbaord fmt todos;

        List.iter (fun line -> Format.fprintf fmt "%s@." line) after_dashboard;

        Format.pp_print_flush fmt ();
        write buff
      in
      match result with
      | Ok () -> ()
      | Error e -> Printf.eprintf "%s\n" (Readme_io.error_message e))
