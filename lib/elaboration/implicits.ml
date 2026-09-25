open Utils
open State

type t = bool list Id_map.t

let empty = Id_map.empty

let set st id flags =
  let implicits = Id_map.insert st.implicits id flags in
  State.with_implicits st implicits

let find id t = Id_map.find t id

let flag_at pos flags =
  let flag = List.nth_opt flags pos in
  Option.value flag ~default:false

let is_implicit st id pos =
  let flags = find id st.implicits in
  let flag = Option.map (flag_at pos) flags in
  Option.value flag ~default:false
