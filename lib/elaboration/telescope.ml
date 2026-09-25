let is_implicit ({ value = annot; _ } : Syntax.annotation) = annot.implicit

let implicits ({ value = tele; _ } : Syntax.telescope) =
  List.map is_implicit tele

let elaborate_annotation st ({ value = annot; _ } : Syntax.annotation) =
  let st, ty = Expression.elaborate st annot.typ in
  let st = Local_context.add st annot.name.value ty in
  Occurrences.variable st ty annot.name.span;
  (st, ty)

let elaborate st ({ value = tele; _ } : Syntax.telescope) =
  State.fold st elaborate_annotation tele
