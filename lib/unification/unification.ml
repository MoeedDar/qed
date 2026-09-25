type solution = {
  terms : Core.Term_meta_context.t;
  levels : Core.Level_meta_context.t;
  failed : Core.Constraints.t list;
  postponed : Core.Constraints.t list;
}

let solve terms levels environment (constraints : Core.Constraints.t list) =
  let unlocated = List.map Span.unlocate constraints in
  let ctx = { Context.terms; levels; environment } in
  let ctx, failed, postponed = Solve.solve ctx unlocated in
  let failed = Span.pick constraints failed in
  let postponed = Span.pick constraints postponed in
  { terms = ctx.terms; levels = ctx.levels; failed; postponed }
