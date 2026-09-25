open Core
open State

let add st nm decl =
  let nm = Resolve.qualify st nm in
  let env = Environment.add st.environment decl in
  let gc = Global_context.add st.global_context nm env.id in
  let st = State.with_environment st env in
  let st = State.with_global_context st gc in
  (st, env.id)

let add_definition st flags nm params term typ =
  let term = Term_meta_context.instantiate st.term_meta_context term in
  let params =
    List.map (Term_meta_context.instantiate st.term_meta_context) params
  in
  let typ = Term_meta_context.instantiate st.term_meta_context typ in
  let decl = Declaration.make_define params term typ in
  let st, id = add st nm decl in
  let st = Implicits.set st id flags in
  (st, id, Term.constant id [])

let begin_inductive st nm ty =
  let dummy = Declaration.make_define [] Term.Bad ty in
  let st, id = add st nm dummy in
  let st = State.with_namespace st nm in
  (st, id)

let finish_inductive st id typ params fam ctors flags =
  let elim_decl = Declaration.make_elim id params fam ctors [] in
  let st, elim_id = add st "elim" elim_decl in
  let decl = Declaration.make_inductive typ ctors elim_id in
  let env = Environment.set st.environment id decl in
  let st = State.with_environment st env in
  let st = Implicits.set st id flags in
  (st, Term.constant id [])

let add_constructor st flags nm ind pos ind_params params body =
  let ctor = Core.Constructor.make ind pos ind_params params body in
  let decl = Declaration.Constructor ctor in
  let st, id = add st nm decl in
  let st = Implicits.set st id flags in
  (st, (id, ctor))
