type t' =
  | Hole
  | Symbol of string
  | Natural of int
  (* delimiters *)
  | Left_parenthesis
  | Right_parenthesis
  | Left_curly_brace
  | Right_curly_brace
  (* operators *)
  | Assign
  | Fat_arrow
  | Arrow
  | Bar
  | Colon
  (* keywords *)
  | Let
  | In
  | Case
  | Def

type t = t' Span.located

let of_text = function
  | "let" -> Let
  | "in" -> In
  | "case" -> Case
  | "def" -> Def
  | s -> Symbol s

let of_digit s = Natural (int_of_string s)
