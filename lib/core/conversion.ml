let equal tmc env tm tm' =
  let norm = Reduction.normalise tmc env in
  let tm = norm tm in
  let tm' = norm tm' in
  Term.Relation.equals tm tm'
