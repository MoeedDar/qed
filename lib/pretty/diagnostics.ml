open Source.Diagnostics

type context = {
  global_context : Core.Global_context.t;
  local_context : Core.Local_context.t;
  terms : Core.Term_meta_context.t;
}

let term ctx tm = Render.term ctx.global_context ctx.local_context tm
let level l = Term.level l
let severity _ = 1

let message ctx = function
  | Expected_closing_bracket -> "expected a closing bracket"
  | Unexpected_closing_bracket -> "unexpected closing bracket"
  | Mismatched_bracket c -> Printf.sprintf "mismatched bracket, expected `%c`" c
  | Invalid_operator -> "invalid operator"
  | Expected_expression_before x -> "expected an expression before " ^ x
  | Expected_expression_after x -> "expected an expression after " ^ x
  | Expected_marker x -> "expected marker `" ^ x ^ "`"
  | Unexpected_expression -> "unexpected expression"
  | Unexpected_declaration -> "unexpected definition or case"
  | Expected_name -> "expected name"
  | Expected_annotation -> "expected a binder"
  | Expected_scrutinee -> "expected a parameter to split on"
  | Unbound_reference x -> "unbound reference `" ^ x ^ "`"
| Expected_constructor -> "unknown constructor"
| Inexhaustive_match n -> Printf.sprintf "case expression is not exhaustive; %d constructor(s) missing" n
| Unsupported_wildcard -> "wildcard patterns are not supported"
| Expected_inductive ty ->
    "expected an inductive type, but found " ^ term ctx ty
| Expected_pi ty -> "expected a function type, but found " ^ term ctx ty
| Expected_sort ty -> "expected a type, but found " ^ term ctx ty
| Type_mismatch (actual, expected) ->
    let expected = term ctx expected in
    let actual = term ctx actual in
    Printf.sprintf "type mismatch: expected %s, found %s" expected actual
| Universe_mismatch (left, right) ->
    Printf.sprintf "universe mismatch: %s and %s are not in the same universe"
      (level left) (level right)
| Internal_error x -> "internal error: " ^ x
