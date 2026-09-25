open Core
open State

let whnf st tm =
  let find_term = Term_meta_context.find_term st.term_meta_context in
  Core.Reduction.whnf find_term st.environment tm
