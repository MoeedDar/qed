open Core
open State

let fresh_term st typ span =
  let mctx =
    Term_meta_context.fresh st.term_meta_context st.local_context typ span
  in
  let st = State.with_term_meta_context st mctx in
  (st, Term.Meta_variable mctx.id)

let fresh_level st =
  let mctx = Level_meta_context.fresh st.level_meta_context in
  let st = State.with_level_meta_context st mctx in
  (st, Level.Meta_variable mctx.id)

let fresh_type st span =
  let st, level = fresh_level st in
  fresh_term st (Term.Sort level) span
