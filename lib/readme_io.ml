type readme_error =
  | Not_found of string
  | Read_error of string
  | Write_error of string

let error_message (e : readme_error) : string =
  match e with Not_found msg | Read_error msg | Write_error msg -> msg

let default_file_path = "README.md"

let read ?(file_path = default_file_path) () :
    (string list, readme_error) result =
  let open In_channel in
  match with_open_text file_path input_lines with
  | lines -> Ok lines
  | exception Sys_error msg ->
      if not (Sys.file_exists file_path) then begin
        Printf.eprintf "%s not found: creating\n" file_path;
        Ok []
      end
      else Error (Read_error msg)

let write ?(file_path = default_file_path) (buff : Buffer.t) :
    (unit, readme_error) result =
  let open Out_channel in
  let write_fn (oc : Out_channel.t) =
    Buffer.output_buffer oc buff;
    Out_channel.flush oc
  in
  match
    with_open_gen
      [ Open_text; Open_wronly; Open_creat ]
      0o644 file_path write_fn
  with
  | exception Sys_error msg -> Error (Write_error msg)
  | _ -> Ok ()
