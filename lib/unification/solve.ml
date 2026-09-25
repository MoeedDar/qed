open Outcome
module Constraints = Core.Constraints

let attempt ctx constr =
  match constr with
  | Constraints.Conversion (left, right) -> Term.unify ctx left right
  | Constraints.Universe_inequality (left, right) -> Level.unify ctx left right

let step (ctx, progress, postponed, failed) constr =
  let outcome = attempt ctx constr in
  match outcome.status with
  | Solved -> (outcome.ctx, true, postponed, failed)
  | Postponed -> (ctx, progress, constr :: postponed, failed)
  | Failed -> (ctx, progress, postponed, constr :: failed)

let pass ctx constrs =
  let initial = (ctx, false, [], []) in
  let ctx, progress, postponed, failed = List.fold_left step initial constrs in
  (ctx, progress, List.rev postponed, List.rev failed)

let rec loop ctx failed postponed consts =
  let ctx, solved, next_postponed, next_failed = pass ctx consts in
  let failed = next_failed @ failed in
  let postponed = next_postponed @ postponed in
  if solved then loop ctx failed postponed next_postponed
  else (ctx, failed, postponed)

let solve ctx consts = loop ctx [] [] consts
