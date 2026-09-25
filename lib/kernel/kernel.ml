open Core
open Utils

let tmc _ = None

type error =
  | No_meta
  | No_bad
  | Mismatch of Term.t * Term.t
  | Expected_sort of Term.t
  | Expected_pi of Term.t

exception Error of error

let whnf = Reduction.whnf tmc
let equal = Conversion.equal tmc

let rec check env ctx actual expected =
  let actual' = infer env ctx actual in
  if equal env actual' expected then ()
  else raise (Error (Mismatch (actual', expected)))

and infer env ctx = function
  | Term.Bad -> raise (Error No_bad)
  | Term.Meta_variable _ -> raise (Error No_meta)
  | Term.Sort l -> Term.Sort (Level.Succ l)
  | Term.Variable i -> Context.get ctx i
  | Term.Constant (i, _) -> (
      match Environment.get_type env i with
      | Some ty -> ty
      | None -> raise (Error No_bad))
  | Term.Pi (a, b) ->
      let ctx' = Context.add ctx a in
      let i = infer_sort env ctx a in
      let j = infer_sort env ctx' b in
      Term.Sort (Level.Max (i, j))
  | Term.Lambda (a, b) ->
      let ctx' = Context.add ctx a in
      let _ = infer_sort env ctx a in
      let b = infer env ctx' b in
      Term.Pi (a, b)
  | Term.Application (f, x) -> (
      let typ = infer_whnf env ctx f in
      match typ with
      | Term.Pi (a, b) ->
          let _ = check env ctx x a in
          Substitution.instantiate b x
      | _ -> raise (Error (Expected_pi typ)))

and infer_sort env ctx tm =
  match infer_whnf env ctx tm with
  | Term.Sort l -> l
  | _ -> raise (Error (Expected_sort tm))

and infer_whnf env ctx tm = infer env ctx tm |> whnf env

let declaration_pending = function
  | Declaration.Define d ->
      Term.Meta_variable.contains d.term || Term.Meta_variable.contains d.typ
  | _ -> false

let pending env =
  Id_map.fold env
    (fun _ declaration acc -> acc || declaration_pending declaration)
    false

let check_declarations env =
  Id_map.fold env
    (fun _ declaration found ->
      match found with
      | Some _ -> found
      | None -> (
          match declaration with
          | Declaration.Define d -> (
              try
                check env Context.empty d.term d.typ;
                None
              with Error error -> Some error)
          | _ -> None))
    None
