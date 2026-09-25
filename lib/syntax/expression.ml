type t' =
  | Bad
  | Hole
  | Natural of int
  | Symbol of string
  | Application of t * t
  | Assign of t * t
  | Fat_arrow of t * t
  | Arrow of t * t
  | Bar of t * t
  | Colon of t * t
  | Let of t * t * t
  | Case of t * t
  | Curly_brace of t

and t = t' Span.located
