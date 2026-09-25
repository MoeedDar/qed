open Utils

type t = Level.t Id_map.t

let empty : t = Id_map.empty
let fresh t = Id_map.with_next t
let find t id = Id_map.find t id
let set t id level = Id_map.map_add t id level

let force t level =
  match level with
  | Level.Meta_variable id -> Option.value (find t id) ~default:level
  | _ -> level
