module String = struct
  include String

  let trim_left (s: string): string =
    let len = String.length s in
    let rec find_start i =
      if i >= len then len
      else match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> find_start (i + 1)
      | _ -> i
    in let start = find_start 0
    in
    if start = 0 then s
    else String.sub s start (len - start)
end
