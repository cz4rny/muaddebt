type readme_error =
  | Not_found of string
  | Read_error of string
  | Write_error of string

let error_message (e : readme_error) : string =
  match e with Not_found msg | Read_error msg | Write_error msg -> msg

let default_file_path = "README.md"

let update_readme_lines ?(file_path = default_file_path)
    (f : Out_channel.t -> string option -> (unit, readme_error) result) =
  let filename = Filename.basename file_path in
  let ext = Filename.extension filename in
  let tmp_readme =
    Filename.temp_file (Filename.remove_extension filename) ext
  in

  let inch : In_channel.t option ref = ref None in
  let outch : Out_channel.t option ref = ref None in
  Fun.protect
    begin fun () ->
      let open Result.Syntax in
      let process_lines ic oc =
        let rec loop () =
          match In_channel.input_line ic with
          | Some line ->
              let* () = f oc (Some line) in
              loop ()
          | None -> f oc None
        in
        loop ()
      in

      let* oc =
        try Ok (Out_channel.open_text tmp_readme)
        with Sys_error msg -> Error (Write_error msg)
      in
      outch := Some oc;

      let* file_exists =
        try Ok (Sys.file_exists file_path)
        with Sys_error msg -> Error (Read_error msg)
      in

      let* () =
        match file_exists with
        | true ->
            let* ic =
              try Ok (In_channel.open_text file_path)
              with Sys_error msg -> Error (Read_error msg)
            in
            inch := Some ic;

            process_lines ic oc
        | false -> f oc None
      in

      try
        Out_channel.flush oc;
        Sys.rename tmp_readme file_path;
        Ok ()
      with Sys_error msg -> Error (Write_error msg)
    end ~finally:begin fun () ->
    Option.iter In_channel.close_noerr !inch;
    Option.iter Out_channel.close_noerr !outch
    end

type mode = EmptyFile | Copy | Skip

let update_readme ?(file_path = default_file_path) (todos : Todo.t list) :
    (unit, readme_error) result =
  let mode = ref EmptyFile in
  let dashboard_inserted = ref false in
  update_readme_lines ~file_path begin fun out_channel line ->
      match (!mode, line) with
      | EmptyFile, Some line when Dash_md.is_header line ->
          mode := Skip;
          Ok ()
      | EmptyFile, Some line ->
          mode := Copy;
          Ok (Out_channel.output_string out_channel (line ^ "\n"))
      | Copy, Some line when Dash_md.is_header line ->
          mode := Skip;
          Ok ()
      | Copy, Some line ->
          Out_channel.output_string out_channel (line ^ "\n");
          Ok ()
      | Skip, Some line when Dash_md.is_footer line ->
          mode := Copy;
          let fmt = Format.formatter_of_out_channel out_channel in
          Dash_md.pp_dashbaord fmt todos;
          dashboard_inserted := true;
          Ok ()
      | Skip, Some _ -> Ok ()
      | _, None when not !dashboard_inserted ->
          let fmt = Format.formatter_of_out_channel out_channel in
          if !mode <> EmptyFile then Format.pp_print_newline fmt ();
          Ok (Dash_md.pp_dashbaord fmt todos)
      | _, None -> Ok ()
    end
