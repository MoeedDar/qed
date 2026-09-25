let rec equal tmc env tm tm' =
  let whnf = Reduction.whnf tmc env in
  (whnf tm, whnf tm') |> Term.Relation.lift (equal tmc env)
