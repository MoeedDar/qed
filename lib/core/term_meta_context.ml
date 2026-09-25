open Local_context
open Utils

type entry = {
  local : Local_context.t;
  typ : Term.t;
  term : Term.t option;
  span : Span.t;
}

let entry local typ span = { local; typ; term = None; span }
let term e = e.term

type t = entry Id_map.t

let empty = Id_map.empty
let fresh t lc ty span = entry lc ty span |> Id_map.add t
let find = Id_map.find
let find_term t id = find t id |> Option.map term |> Option.join

let set t id tm =
  let entry = Option.get (find t id) in
  Id_map.set t id { entry with term = Some tm }

let context e = e.local.types

let force t tm =
  match tm with
  | Term.Meta_variable id -> Option.value (find_term t id) ~default:tm
  | _ -> tm

let rec instantiate t tm =
  match tm with
  | Term.Meta_variable id -> (
      match find_term t id with Some value -> instantiate t value | None -> tm)
  | Pi (a, b) -> Pi (instantiate t a, instantiate t b)
  | Lambda (a, b) -> Lambda (instantiate t a, instantiate t b)
  | Application (a, b) ->
      Application (instantiate t a, instantiate t b)
  | tm -> tm
