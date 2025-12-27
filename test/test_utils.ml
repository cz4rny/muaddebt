let or_fail_with ~pp_error (msg : string) = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: %s" msg (pp_error e)

let or_fail_with_exn (msg : string) f =
  try f () with e -> Alcotest.failf "%s: %s" msg (Printexc.to_string e)

let visible_string =
  Alcotest.testable
    (fun ppf s ->
      let buf = Buffer.create (String.length s * 2) in
      String.iter
        (function
          | '\n' -> Buffer.add_string buf "↵\n"
          | '\t' -> Buffer.add_string buf "→"
          | ' ' -> Buffer.add_string buf "·"
          | c -> Buffer.add_char buf c)
        s;
      Fmt.string ppf (Buffer.contents buf))
    String.equal

let read_whole_file (filepath : string) =
  (fun () -> In_channel.with_open_text filepath In_channel.input_all)
  |> or_fail_with_exn ("failed to read updated readme " ^ filepath)

let open_temp_file ?(filename = "README.md") () =
  let filename_without_extension = Filename.remove_extension filename in
  let extension = Filename.extension filename in
  (fun () -> Filename.open_temp_file filename_without_extension extension)
  |> or_fail_with_exn "failed to create temp file"

let with_temp_file ?(filename = "README.md")
    ?(init_content : string option = None)
    (f : file_path:string -> Out_channel.t -> unit) =
  let file_path, oc = open_temp_file ~filename () in
  Option.iter
    (fun content ->
      (fun () ->
        Out_channel.output_string oc content;
        Out_channel.flush oc)
      |> or_fail_with_exn ("failed to write initial content to " ^ file_path))
    init_content;
  Fun.protect
    ~finally:(fun () ->
      Out_channel.close_noerr oc;
      try Sys.remove file_path
      with exn ->
        Printf.eprintf "Warning: failed to remove %s on cleanup: %s\n%!"
          file_path (Printexc.to_string exn))
    (fun () -> f ~file_path oc)
