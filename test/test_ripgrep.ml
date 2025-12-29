open Muaddebt
open Muaddebt.Ripgrep.Private

let test_strips_end_comment_marks () =
  let test_parse_line input expected = 
    parse_rg_line input
      |> Option.map (fun (todo: Todo.t) -> 
        Printf.sprintf "%s: %s" (Marker.to_string todo.marker) todo.msg)
      |> Option.get
      |> Alcotest.(check string) "same string" expected
  in

  test_parse_line "nofile\x001:1: TODO: ocaml comment *)"       "TODO: ocaml comment";
  test_parse_line "nofile\x001:1: FIXME: C-style comment	*/  " "FIX: C-style comment";
  test_parse_line "nofile\x001:1: BUG: HTML-like comment-->	"   "BUG: HTML-like comment";
