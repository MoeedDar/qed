module Level = Core.Level
module Level_meta_context = Core.Level_meta_context
open Context
open Outcome

let assign ctx id level =
  let levels = Level_meta_context.set ctx.levels id level in
  solved { ctx with levels }

let outcome_for ctx left right =
  let contains = Level.Meta_variable.contains in
  if contains left || contains right then postponed ctx else failed ctx

let rec unify ctx left right =
  let left = Level_meta_context.force ctx.levels left in
  let right = Level_meta_context.force ctx.levels right in
  if Level.Relation.equals left right then solved ctx
  else
    match (left, right) with
    | Level.Meta_variable m, level | level, Level.Meta_variable m ->
        if Level.Meta_variable.occurs m level then failed ctx
        else assign ctx m level
    | Succ a, Succ b -> unify ctx a b
    | Max (a, b), Max (a', b') ->
        let outcome = unify ctx a a' in
        halt outcome (fun ctx -> unify ctx b b')
    | _ -> outcome_for ctx left right

let rec unify_all ctx left right =
  match (left, right) with
  | [], [] -> solved ctx
  | left_head :: left_tail, right_head :: right_tail ->
      let outcome = unify ctx left_head right_head in
      halt outcome (fun ctx -> unify_all ctx left_tail right_tail)
  | [], _ :: _ | _ :: _, [] -> failed ctx
