open Span

let all_implicit n =
  let rec go n = if n <= 0 then [] else true :: go (n - 1) in
  go n

let implicit_flags ind_params (ctor : Syntax.constructor') =
  let ctor_flags = Telescope.implicits ctor.parameters in
  all_implicit (List.length ind_params) @ ctor_flags

let elaborate ind ind_params st pos ({ value = ctor; _ } : Syntax.constructor) =
  let st, params = Telescope.elaborate st ctor.parameters in
  let st, body = Expression.elaborate st ctor.body in
  let flags = implicit_flags ind_params ctor in
  Declare.add_constructor st flags ctor.name.value ind pos ind_params params body

let elaborate_all st ind ind_params ctors =
  State.foldi st (elaborate ind ind_params) ctors.value
