type t' =
  | Symbol of string
  | Natural of int
  | Hole
  | Left_parenthesis
  | Right_parenthesis
  | Left_curly_brace
  | Right_curly_brace
  | Assign
  | Fat_arrow
  | Arrow
  | Bar
  | Colon
  | Let
  | In
  | Case

type t = t' Span.located
