type t = Todo | Fix | Bug | Hack

(* The Single Source of Truth *)
(* Format: (Variant, Canonical Name, [Aliases]) *)
let definition = [
  (Todo, "TODO", [||]);
  (Fix , "FIX" , [| "FIXME" |]);
  (Bug , "BUG" , [||]);
  (Hack, "HACK", [||]);
]

(* Flattens the definition into [ ("TODO", Todo); ("FIX", Fix); ("FIXME", Fix)... ] *)
let all_strings : (string * t) array =
  let count = 
    List.fold_left (fun acc (_, _, aliases) -> acc + 1 + Array.length aliases) 0 definition 
  in
  let arr = Array.make count ("", Todo) in
  
  let idx = ref 0 in
  definition |> List.iter (fun (variant, canonical, aliases) ->
    arr.(!idx) <- (canonical, variant);
    incr idx;

    aliases |> Array.iter (fun alias ->
      arr.(!idx) <- (alias, variant);
      incr idx
    )
  );
  arr

let to_string t =
  (* Since this is only for tests/debug, a simple list lookup is fine. *)
  (* It's O(N) but N=4, so it's instant. *)
  match List.find_opt (fun (v, _, _) -> v = t) definition with
  | Some (_, name, _) -> name
  | None -> "???"
