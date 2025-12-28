type process_error =
  | Launch_failed    of string
  (* | Read_failed   of string *)
  | Execution_failed of int

val find_todos : bool -> (Todo.t list, process_error) result
