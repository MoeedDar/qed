open Utils

module Argument = struct
  type t = { typ : Term.t; indices : Term.t list option }

  let typ arg = arg.typ
  let indices arg = arg.indices
  let is_recursive arg = Option.is_some arg.indices

  let make_recursive params typ =
    let indices = Term.Application.arguments typ |> drop params in
    { typ; indices = Some indices }

  let make ind params typ =
    if Term.Constant.occurs ind typ then make_recursive params typ
    else { typ; indices = None }
end

type t = {
  inductive_id : int;
  typ : Term.t;
  position : int;
  arguments : Argument.t list;
  codomain_arguments : Term.t list;
}

let recursive_arguments ctor =
  ctor.arguments
  |> List.mapi (fun i arg -> (i, arg))
  |> List.filter (fun (_, arg) -> Argument.is_recursive arg)
  |> List.map (fun (i, arg) -> (i, Option.get arg.Argument.indices))

let make inductive_id position inductive_params parameters body =
  let ind_n = List.length inductive_params in
  let all = inductive_params @ parameters in
  let typ = Term.Pi.fold all body in
  let arg = Argument.make inductive_id ind_n in
  let arguments = typ |> Term.Pi.domains |> drop ind_n |> List.map arg in
  let codomain_arguments =
    Term.Pi.codomains typ |> Term.Application.arguments |> drop ind_n
  in
  { typ; inductive_id; position; arguments; codomain_arguments }
