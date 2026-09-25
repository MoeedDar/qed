let global_name (global_context : Core.Global_context.t) id =
  match Core.Global_context.name global_context id with
  | Some n -> n
  | None -> "?" ^ string_of_int id

let local_name (local_context : Core.Local_context.t) i =
  Core.Local_context.find_name local_context i

let term global_context local_context tm =
  Term.term (global_name global_context) (local_name local_context) tm
