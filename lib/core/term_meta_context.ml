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
let context e = e.local.types
let with_term e term = { e with term = Some term }

type t = entry Id_map.t

let empty = Id_map.empty
let fresh t lc ty span = entry lc ty span |> Id_map.add t
let find = Id_map.find
let find_term t id = find t id |> Option.map term |> Option.join

let set t id tm =
  let entry = Option.get (find t id) in
  Id_map.set t id (with_term entry tm)

let force t = function
  | Term.Meta_variable id as tm -> Option.value (find_term t id) ~default:tm
  | tm -> tm

let rec instantiate t = function
  | Term.Meta_variable id as tm -> (
      match find_term t id with Some x -> instantiate t x | None -> tm)
  | Pi (a, b) -> Pi (instantiate t a, instantiate t b)
  | Lambda (a, b) -> Lambda (instantiate t a, instantiate t b)
  | Application (a, b) -> Application (instantiate t a, instantiate t b)
  | tm -> tm

let unsolved t =
  let f id e acc = match term e with None -> (id, e) :: acc | Some _ -> acc in
  Id_map.fold t f []

let live t roots =
  unsolved t
  |> List.filter (fun (id, _) ->
      List.exists (Term.Meta_variable.occurs id) roots)
