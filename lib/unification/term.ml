module Term = Core.Term
module Term_meta_context = Core.Term_meta_context
module Substitution = Core.Substitution
module Reduction = Core.Reduction
open Context
open Outcome

let term_metas ctx id = Term_meta_context.find_term ctx.terms id

let rec in_scope depth solution =
  match solution with
  | Term.Variable i -> i < depth
  | Term.Pi (a, b) | Term.Lambda (a, b) | Term.Application (a, b) ->
      in_scope depth a && in_scope (depth + 1) b
  | _ -> true

let scope_depth meta = List.length (Term_meta_context.context meta)

let assign ctx id solution =
  let meta = Term_meta_context.find ctx.terms id in
  match meta with
  | None -> failed ctx
  | Some meta ->
      let occurs = Term.Meta_variable.occurs id solution in
      let scoped = in_scope (scope_depth meta) solution in
      if occurs || not scoped then failed ctx
      else
        let terms = Term_meta_context.set ctx.terms id solution in
        solved { ctx with terms }

let distinct variables =
  match List.sort_uniq compare variables with
  | sorted -> List.length sorted = List.length variables

let solve_pattern ctx id arguments right =
  let as_variable = function Term.Variable i -> Some i | _ -> None in
  let variables = List.filter_map as_variable arguments in
  let all_variables = List.length variables = List.length arguments in
  if not all_variables then postponed ctx
  else if not (distinct variables) then postponed ctx
  else
    let meta = Term_meta_context.find ctx.terms id in
    match meta with
    | None -> failed ctx
    | Some meta ->
        let binder_types =
          List.map
            (fun i -> Core.Context.get (Term_meta_context.context meta) i)
            variables
        in
        let abstracted = Substitution.abstract variables right in
        let solution = Term.Lambda.fold binder_types abstracted in
        assign ctx id solution

let rec unify_meta ctx id arguments right =
  match term_metas ctx id with
  | Some head ->
      let applied = Term.Application.fold head arguments in
      unify ctx applied right
  | None -> solve_pattern ctx id arguments right

and unify_binder ctx a b a' b' =
  let outcome = unify ctx a a' in
  halt outcome (fun ctx -> unify ctx b b')

and unify_rigid_heads ctx head arguments head' arguments' =
  let outcome = unify ctx head head' in
  halt outcome (fun ctx -> unify_arguments ctx arguments arguments')

and unify_arguments ctx left right =
  match (left, right) with
  | [], [] -> solved ctx
  | left_head :: left_tail, right_head :: right_tail ->
      let outcome = unify ctx left_head right_head in
      halt outcome (fun ctx -> unify_arguments ctx left_tail right_tail)
  | [], _ :: _ | _ :: _, [] -> failed ctx

and unify_rigid ctx left right =
  match (left, right) with
  | Term.Meta_variable m, right | right, Term.Meta_variable m ->
      unify_meta ctx m [] right
  | Term.Sort left_level, Term.Sort right_level ->
      Level.unify ctx left_level right_level
  | Term.Constant (i, left_levels), Term.Constant (j, right_levels) when i = j
    ->
      Level.unify_all ctx left_levels right_levels
  | Term.Pi (a, b), Term.Pi (a', b') -> unify_binder ctx a b a' b'
  | Term.Lambda (a, b), Term.Lambda (a', b') -> unify_binder ctx a b a' b'
  | Term.Application _, _ | _, Term.Application _ ->
      unify_application ctx left right
  | _ -> failed ctx

and unify_application ctx left right =
  let head, arguments = Term.Application.unfold left in
  let head', arguments' = Term.Application.unfold right in
  match (head, head') with
  | Term.Meta_variable m, _ -> unify_meta ctx m arguments right
  | _, Term.Meta_variable m -> unify_meta ctx m arguments' left
  | _ -> unify_rigid_heads ctx head arguments head' arguments'

and whnf ctx term =
  match Reduction.step (term_metas ctx) ctx.environment term with
  | Some reduct -> whnf ctx reduct
  | None -> term

and unify ctx left right =
  let left = Term_meta_context.force ctx.terms left in
  let right = Term_meta_context.force ctx.terms right in
  if Term.Relation.equals left right then solved ctx
  else
    let left = Reduction.norm (term_metas ctx) ctx.environment left in
    let right = Reduction.norm (term_metas ctx) ctx.environment right in
    if Term.Relation.equals left right then solved ctx
    else unify_rigid ctx left right
