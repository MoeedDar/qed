open Core

let ( let* ) = Result.bind

type error =
  | No_meta
  | No_bad
  | Mismatch of Term.t * Term.t
  | Expected_sort of Term.t
  | Expected_pi of Term.t

type 'a t = ('a, error) result

let tmc _ = None
let whnf = Reduction.whnf tmc
let equal = Conversion.equal tmc

let rec check env ctx actual expected =
  let* actual' = infer env ctx actual in
  if equal env actual' expected then Ok ()
  else Error (Mismatch (actual', expected))

and infer env ctx = function
  | Term.Bad -> Error No_bad
  | Term.Meta_variable _ -> Error No_meta
  | Term.Sort l -> Ok (Term.Sort (Level.Succ l))
  | Term.Variable i -> Ok (Context.get ctx i)
  | Term.Constant (i, _) -> Ok (Environment.get_type env i)
  | Term.Pi (a, b) ->
      let* i = infer_sort env ctx a in
      let ctx' = Context.add ctx a in
      let* j = infer_sort env ctx' b in
      Ok (Term.Sort (Level.Max (i, j)))
  | Term.Lambda (a, b) ->
      let* _ = infer_sort env ctx a in
      let ctx' = Context.add ctx a in
      let* b = infer env ctx' b in
      Ok (Term.Pi (a, b))
  | Term.Application (f, x) -> (
      let* typ = infer_whnf env ctx f in
      match typ with
      | Term.Pi (a, b) ->
          let* () = check env ctx x a in
          Ok (Substitution.instantiate b x)
      | _ -> Error (Expected_pi typ))

and infer_sort env ctx tm =
  let* tm' = infer_whnf env ctx tm in
  match tm' with Term.Sort l -> Ok l | _ -> Error (Expected_sort tm')

and infer_whnf env ctx tm =
  let* tm' = infer env ctx tm in
  Ok (whnf env tm')
