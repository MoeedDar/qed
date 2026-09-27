open Utils

type t = Declaration.t Id_map.t

let empty = Id_map.empty
let add = Id_map.add
let set = Id_map.set
let find = Id_map.find
let map f env id = find env id |> Option.map f
let bind f env id = Option.bind (find env id) f
let get_type env id = Id_map.get env id |> Declaration.type_of
let find_type env id = map Declaration.type_of env id
let find_value env id = bind Declaration.value_of env id
let find_eliminator env id = bind Declaration.eliminator_of env id
let find_inductive env id = bind Declaration.inductive_of env id
let find_constructor env id = bind Declaration.constructor_of env id

let find_eliminator_id_of_inductive env ind_id =
  find_inductive env ind_id |> Option.map Inductive.eliminator_id

let terms env =
  let of_declaration acc = function
    | Declaration.Define d -> d.term :: d.typ :: acc
    | Declaration.Inductive i -> i.typ :: acc
    | Declaration.Constructor c ->
        List.fold_left (fun acc arg -> arg.Constructor.Argument.typ :: acc)
          (c.codomain_arguments @ c.typ :: acc)
          c.arguments
    | Declaration.Eliminator e -> e.typ :: acc
  in
  Id_map.fold env (fun _ d acc -> of_declaration acc d) []

let instantiate_declaration f = function
  | Declaration.Define d ->
      Declaration.Define { Define.term = f d.term; typ = f d.typ }
  | Declaration.Inductive i ->
      Declaration.Inductive { i with Inductive.typ = f i.typ }
  | Declaration.Constructor c ->
      let arguments =
        List.map
          (fun (arg : Constructor.Argument.t) ->
            {
              arg with
              Constructor.Argument.indices =
                Option.map (List.map f) arg.Constructor.Argument.indices;
            })
          c.arguments
      in
      Declaration.Constructor
        {
          c with
          Constructor.typ = f c.typ;
          codomain_arguments = List.map f c.codomain_arguments;
          arguments;
        }
  | Declaration.Eliminator e -> Declaration.Eliminator { e with Eliminator.typ = f e.typ }

(* Rewrite every declaration with [f], keeping identifiers stable. Solved
   metavariables are folded away, so the result is the committed, closed
   environment the kernel is meant to see. *)
let instantiate f env =
  Id_map.fold env
    (fun id d acc -> Id_map.insert acc id (instantiate_declaration f d))
    env
