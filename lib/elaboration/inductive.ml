let elaborate st (ind : Syntax.inductive) =
  let st, params = Telescope.elaborate st ind.parameters in
  let st, fam = Expression.elaborate st ind.typ in
  let ty = Core.Term.Pi.fold params fam in
  let st, id = Declare.begin_inductive st ind.name.value ty in
  Occurrences.declaration st id ind.name.span;
  let st, ctors = Constructor.elaborate_all st id params ind.constructors in
  let flags = Telescope.implicits ind.parameters in
  Declare.finish_inductive st id ty params fam ctors flags
