let reset st =
  let st = State.with_local_context st Core.Local_context.empty in
  let st = State.with_self st None in
  let st = State.with_namespace st "" in
  st

let elaborate st cmd =
  let st = reset st in
  match cmd.Span.value with
  | Syntax.Evaluate expr -> Expression.elaborate st expr
  | Syntax.Inductive ind -> Inductive.elaborate st ind
  | Syntax.Define def -> Define.elaborate st def

let elaborate_all st cmds =
  let st, _ = State.fold st elaborate cmds in
  st
