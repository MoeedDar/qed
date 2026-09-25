open State

let add st name ty =
  Core.Local_context.add st.local_context name ty |> State.with_local_context st

let add_with_snapshot st name ty =
  let outer = st.local_context in
  let st = add st name ty in
  (outer, st)

let restore st lc = State.with_local_context st lc
let find_index st name = Core.Local_context.find_index st.local_context name
let get_type st i = Core.Local_context.get_type st.local_context i
let get_name st i = Core.Local_context.get_name st.local_context i
