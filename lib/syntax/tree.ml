type bracket = Parenthesis | Curly_brace

type t' =
  | Symbol of string
  | Name of string
  | Natural of int
  | Hole
  | Assign
  | Fat_arrow
  | Arrow
  | Bar
  | Colon
  | Let
  | In
  | Case
  | List of bracket * t list

and t = t' Span.located
