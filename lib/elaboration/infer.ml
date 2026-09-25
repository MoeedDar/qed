module Term = Core.Term
module Level = Core.Level
module Substitution = Core.Substitution
open Span
open Syntax

let rec level_of_natural n =
  if n = 0 then Level.Zero else Succ (level_of_natural (n - 1))

let get_constant st nm =
  let resolved = Resolve.global st nm in
  let tm, _ = Option.get resolved in
  tm

let rec make_natural n zero succ =
  if n = 0 then zero else Term.Application (succ, make_natural (n - 1) zero succ)

let make_let value_ty body value =
  Term.Application (Lambda (value_ty, body), value)

let rec infer st expr =
  match expr.value with
  | Bad -> infer_bad st
  | Hole -> infer_hole st expr
  | Natural n -> infer_natural st n
  | Variable nm -> infer_variable st expr nm
  | Sort n -> infer_sort_expression st expr n
  | Application (f, x) -> infer_application st expr f x
  | Pi (x, m) -> infer_pi st x m
  | Lambda (x, m) -> infer_lam st x m
  | Let (n, x, m) -> infer_let st n x m
  | Case (scrut, cases) -> infer_case st expr scrut cases

and record_hole st tm =
  match tm with
  | Term.Meta_variable id -> State.with_holes st (id :: st.holes)
  | _ -> st

and infer_bad st = (st, Term.Bad, Term.Bad)

and infer_hole st expr =
  let st, ty = Meta_context.fresh_type st expr.span in
  let st, tm = Meta_context.fresh_term st ty expr.span in
  let st = record_hole st tm in
  (st, tm, ty)

and infer_natural st n =
  let zero = get_constant st "Nat.zero" in
  let succ = get_constant st "Nat.succ" in
  let nat = get_constant st "Nat" in
  let tm = make_natural n zero succ in
  (st, tm, nat)

and infer_variable st expr nm =
  match Resolve.resolve st nm with
  | Some (tm, ty) ->
      Occurrences.variable st ty expr.span;
      (st, tm, ty)
  | None ->
      State.report st (Unbound_reference nm) expr.span;
      (st, Bad, Bad)

and infer_sort_expression st expr n =
  let lvl = level_of_natural n in
  Occurrences.sort st n expr.span;
  (st, Term.Sort lvl, Term.Sort (Level.Succ lvl))

and apply_pi st func arg dom cod =
  let st, arg = check st arg dom in
  let ty = Substitution.instantiate cod arg in
  (st, Term.Application (func, arg), ty)

and apply_meta_variable st expr func func_ty arg =
  let st, dom = Meta_context.fresh_type st expr.span in
  let st, cod = Meta_context.fresh_type st expr.span in
  let st =
    Constraints.add_conversion st func_ty (Term.Pi (dom, cod)) expr.span
  in
  apply_pi st func arg dom cod

and apply_function st expr func func_ty arg =
  match Reduction.whnf st func_ty with
  | Term.Pi (dom, cod) -> apply_pi st func arg dom cod
  | Term.Meta_variable _ -> apply_meta_variable st expr func func_ty arg
  | Term.Bad -> (st, Bad, Bad)
  | _ ->
      State.report st (Expected_pi func_ty) expr.span;
      (st, Bad, Bad)

and infer_application st expr func arg =
  let st, func, func_ty = infer st func in
  let st, func, func_ty = insert_implicits st func func_ty expr.span in
  apply_function st expr func func_ty arg

and infer_domain st (bind : Syntax.annotation) =
  let st, dom, dom_ty = infer st bind.value.typ in
  let st, dom_lvl = infer_sort st dom_ty bind.value.typ.span in
  (st, dom, dom_lvl)

and infer_codomain st (bind : Syntax.annotation) dom body =
  let outer, st =
    Local_context.add_with_snapshot st bind.value.name.value dom
  in
  let st, cod, cod_ty = infer st body in
  let st = Local_context.restore st outer in
  let st, cod_lvl = infer_sort st cod_ty body.span in
  (st, cod, cod_ty, cod_lvl)

and infer_pi st bind body =
  let st, dom, dom_lvl = infer_domain st bind in
  let st, cod, _cod_ty, cod_lvl = infer_codomain st bind dom body in
  let lvl = Level.Max (dom_lvl, cod_lvl) in
  (st, Pi (dom, cod), Sort lvl)

and infer_lam st bind body =
  let st, dom, _dom_lvl = infer_domain st bind in
  let st, body, body_ty, _cod_lvl = infer_codomain st bind dom body in
  (st, Lambda (dom, body), Pi (dom, body_ty))

and infer_let st bind value body =
  let st, value_tm, value_ty = infer st value in
  let outer, st =
    Local_context.add_with_snapshot st bind.value.name.value value_ty
  in
  let st, body_tm, body_ty = infer st body in
  let st = Local_context.restore st outer in
  let tm = make_let value_ty body_tm value_tm in
  let ty = Substitution.instantiate body_ty value_tm in
  (st, tm, ty)

and infer_case st expr scrut cases =
  let st, ty = Meta_context.fresh_type st expr.span in
  let st, tm = Case.elaborate infer check st scrut cases ty expr.span in
  (st, tm, ty)

and infer_sort st ty span =
  match Reduction.whnf st ty with
  | Term.Sort lvl -> (st, lvl)
  | Meta_variable _ ->
      let st, lvl = Meta_context.fresh_level st in
      let st = Constraints.add_conversion st ty (Sort lvl) span in
      (st, lvl)
  | Bad -> (st, Zero)
  | _ ->
      State.report st (Expected_sort ty) span;
      (st, Zero)

and check st expr expected =
  match expr.value with
  | Bad -> check_bad st
  | Hole -> check_hole st expr expected
  | Lambda (bind, body) -> check_lam st expr bind body expected
  | Let (bind, value, body) -> check_let st bind value body expected
  | Case (scrut, cases) -> check_case st expr scrut cases expected
  | _ -> check_default st expr expected

and check_bad st = (st, Term.Bad)

and check_hole st expr expected =
  let st, tm = Meta_context.fresh_term st expected expr.span in
  let st = record_hole st tm in
  (st, tm)

and check_annotation st annot expected =
  let st, actual, _ = infer st annot in
  Constraints.add_conversion st actual expected annot.span

and check_lambda_pi st (bind : Syntax.annotation) body dom cod =
  let st = check_annotation st bind.value.typ dom in
  let outer, st =
    Local_context.add_with_snapshot st bind.value.name.value dom
  in
  let st, body = check st body cod in
  let st = Local_context.restore st outer in
  (st, Term.Lambda (dom, body))

and check_lambda_meta_variable st expr bind body expected =
  let st, dom = Meta_context.fresh_type st bind.span in
  let st, cod = Meta_context.fresh_type st expr.span in
  let st = Constraints.add_conversion st expected (Pi (dom, cod)) expr.span in
  check_lambda_pi st bind body dom cod

and check_lam st expr bind body expected =
  match Reduction.whnf st expected with
  | Term.Pi (dom, cod) -> check_lambda_pi st bind body dom cod
  | Meta_variable _ -> check_lambda_meta_variable st expr bind body expected
  | Bad -> (st, Bad)
  | _ ->
      State.report st (Expected_pi expected) expr.span;
      (st, Bad)

and check_let st bind value body expected =
  let st, value_tm, value_ty = infer st value in
  let outer, st =
    Local_context.add_with_snapshot st bind.value.name.value value_ty
  in
  let st, body = check st body expected in
  let st = Local_context.restore st outer in
  let tm = make_let value_ty body value_tm in
  (st, tm)

and check_case st expr scrut cases expected =
  Case.elaborate infer check st scrut cases expected expr.span

and check_default st expr expected =
  let st, tm, actual = infer st expr in
  let st = Constraints.add_conversion st actual expected expr.span in
  (st, tm)

and implicit_here st func arity =
  match Term.Constant.head func with
  | Some id -> Implicits.is_implicit st id arity
  | None -> false

and insert_implicits st func func_ty span =
  let args = Term.Application.arguments func in
  let arity = List.length args in
  match Reduction.whnf st func_ty with
  | Term.Pi (dom, cod) when implicit_here st func arity ->
      let st, meta = Meta_context.fresh_term st dom span in
      let func = Term.Application (func, meta) in
      let func_ty = Substitution.instantiate cod meta in
      insert_implicits st func func_ty span
  | _ -> (st, func, func_ty)
