let install_self st name params typ =
  let typ = Core.Term.Pi.fold params typ in
  let self = { State.name; typ; term = Core.Term.Bad } in
  State.with_self st (Some self)

let declare st (def : Syntax.define) params term typ =
  let flags = Telescope.implicits def.parameters in
  let st, id, tm =
    Declare.add_definition st flags def.name.value params term typ
  in
  Occurrences.declaration st id def.name.span;
  (st, tm)

let elaborate_body st (def : Syntax.define) =
  let st, params = Telescope.elaborate st def.parameters in
  let st, typ = Expression.elaborate st def.typ in
  let st = install_self st def.name.value params typ in
  let st, body = Infer.check st def.body typ in
  (st, params, body, typ)

let elaborate st (def : Syntax.define) =
  let st, params, body, typ = elaborate_body st def in
  declare st def params body typ
