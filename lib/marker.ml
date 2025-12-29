type t = Todo | Fix | Bug | Hack [@@deriving enum]

let info = function
  | Todo -> ("TODO", [||])
  | Fix  -> ("FIX" , [| "FIXME" |])
  | Bug  -> ("BUG" , [||])
  | Hack -> ("HACK", [||])

let all = 
  Array.init (max + 1) (fun i -> 
    match of_enum i with
    | Some x -> x
    | None -> failwith "Logic Error: Enum has gaps or ppx failed"
  )

let to_string m = fst (info m)

let all_strings = 
  let dyn: (string * t) Dynarray.t = Dynarray.create () in
  (* Heuristic: assume ~2 strings per item on average *)
  Dynarray.ensure_capacity dyn ((max + 1) * 2);

  all |> Array.iter (fun m -> 
    let canonical, aliases = info m in
    Dynarray.add_last dyn (canonical, m);
    aliases |> Array.iter (fun a -> Dynarray.add_last dyn (a, m));
  );
  Dynarray.to_array dyn
