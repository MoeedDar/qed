type t' =
  | Conversion of Term.t * Term.t
  | Universe_inequality of Level.t * Level.t

type t = t' Span.located

let rigid = function
  | Conversion (a, b) ->
      (not (Term.Meta_variable.contains a))
      && not (Term.Meta_variable.contains b)
  | Universe_inequality (a, b) ->
      (not (Level.Meta_variable.contains a))
      && not (Level.Meta_variable.contains b)
