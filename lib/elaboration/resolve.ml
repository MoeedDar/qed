module Term = Core.Term
module Environment = Core.Environment
module Global_context = Core.Global_context
open Utils
open State

let qualify st nm = if st.namespace = "" then nm else st.namespace ^ "." ^ nm

let global st nm =
  let* id = Global_context.find st.global_context nm in
  let* ty = Environment.get_type st.environment id in
  Some (Term.Constant (id, []), ty)

let global_qualified st nm = global st (qualify st nm)

let local st nm =
  let* i = Local_context.find_index st nm in
  let typ = Local_context.get_type st i in
  Some (Term.Variable i, typ)

let self st nm =
  let* s = st.self in
  if s.name = nm && s.term <> Term.Bad then Some (s.term, s.typ) else None

let resolve st = local st <|> self st <|> global st <|> global_qualified st
