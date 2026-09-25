open Utils

let iota env tm =
  let head, args = Term.Application.unfold tm in

  let* elim_id = Term.Constant.id head in
  let* elim = Environment.get_eliminator env elim_id in

  let major_pos = Eliminator.major_position elim in
  let* major = List.nth_opt args major_pos in

  let* ctor_id = Term.Constant.head major in
  let* ctor = Environment.get_constructor env ctor_id in

  let minor_pos = Eliminator.minor_position elim ctor.position in
  let* minor = List.nth_opt args minor_pos in

  Some (Eliminator.reduce elim head args ctor major minor)

let delta env = Term.Constant.bind (Environment.get_value env)
let beta tm = Term.Application.map_lambda Substitution.instantiate tm
let meta tmc = Term.Meta_variable.bind tmc
let step tmc env tm = (iota env <|> delta env <|> beta <|> meta tmc) tm
let whnf tmc env tm = repeat (step tmc env) tm

let rec norm tmc env tm =
  let norm = norm tmc env in
  let tm = Term.map norm tm in
  let tm' = step tmc env tm in
  match tm' with Some tm -> norm tm | None -> tm
