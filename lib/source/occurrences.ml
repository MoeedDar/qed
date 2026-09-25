open Core
open Span

type occurrence' =
  | Variable of { typ : Term.t; local_context : Local_context.t }
  | Sort of int
  | Declaration of int

let variable typ lc span = locate (Variable { typ; local_context = lc }) span
let sort sort span = locate (Sort sort) span
let declaration id span = locate (Declaration id) span

type occurrence = occurrence' Span.located
type t = occurrence list ref

let create () : t = ref []
let add t occ = t := occ :: !t
let record t value span = locate value span |> add t
let record_variable t typ lc span = variable typ lc span |> add t
let record_sort t lvl span = sort lvl span |> add t
let record_declaration t id span = declaration id span |> add t

let find ts offset =
  let in_range span offset = span.start <= offset && offset <= span.stop in
  let rec go offset = function
    | [] -> None
    | ({ span; _ } as occ) :: rest ->
        if in_range span offset then Some occ else go offset rest
  in
  go offset !ts
