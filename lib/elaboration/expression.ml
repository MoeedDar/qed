let elaborate st expr =
  let st, expr, _ = Infer.infer st expr in
  (st, expr)
