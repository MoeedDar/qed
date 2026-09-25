open Utils

type t = Declaration.t Id_map.t

let empty = Id_map.empty
let add = Id_map.add
let set = Id_map.set
let find = Id_map.find
let map f env id = find env id |> Option.map f
let bind f env id = Option.bind (find env id) f
let find_type env id = map Declaration.type_of env id
let find_value env id = bind Declaration.value_of env id
let find_eliminator env id = bind Declaration.eliminator_of env id
let find_inductive env id = bind Declaration.inductive_of env id
let find_constructor env id = bind Declaration.constructor_of env id
let get_type env id = map Declaration.type_of env id
let get_value env id = bind Declaration.value_of env id
let get_inductive env id = bind Declaration.inductive_of env id
let get_constructor env id = bind Declaration.constructor_of env id
let get_eliminator env id = bind Declaration.eliminator_of env id

let find_eliminator_id_of_inductive env ind_id =
  get_inductive env ind_id |> Option.map Inductive.eliminator_id
