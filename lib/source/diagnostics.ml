open Core

type message' =
  (* token tree errors *)
  | Expected_closing_bracket
  | Unexpected_closing_bracket
  | Mismatched_bracket of char
  (* operator parser errors *)
  | Invalid_operator
  | Expected_expression_before of string
  | Expected_expression_after of string
  | Expected_marker of string
  (* syntax parser errors *)
  | Unexpected_expression
  | Unexpected_declaration
  | Expected_name
  | Expected_annotation
  | Expected_scrutinee
  (* elaboration errors *)
| Unbound_reference of string
| Expected_constructor
| Inexhaustive_match of int
| Unsupported_wildcard
| Expected_inductive of Term.t
| Expected_pi of Term.t
| Expected_sort of Term.t
  (* unification errors *)
  | Type_mismatch of Term.t * Term.t
  | Universe_mismatch of Level.t * Level.t
  (* others *)
  | Internal_error of string

type message = message' Span.located
type t = message list ref

let create () = ref []
let report ds (msg : message') span = ds := Span.locate msg span :: !ds
let diagnostics ds = List.rev !ds
