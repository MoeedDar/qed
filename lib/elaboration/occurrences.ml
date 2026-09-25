open State
open Source.Occurrences

let record st value span = record st.occurrences value span

let variable st typ span =
  record_variable st.occurrences typ st.local_context span

let sort st lvl span = record_sort st.occurrences lvl span
let declaration st id span = record_declaration st.occurrences id span
