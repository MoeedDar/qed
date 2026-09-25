open State
open Core

let add_conversion st lhs rhs span =
  let equation = Constraints.Conversion (lhs, rhs) in
  let located = Span.locate equation span in
  State.with_constraints st (located :: st.constraints)
