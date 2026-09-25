type name = string Span.located

and expression' =
  | Bad
  | Hole
  | Natural of int
  | Variable of string
  | Sort of int
  | Application of expression * expression
  | Pi of annotation * expression
  | Lambda of annotation * expression
  | Let of annotation * expression * expression
  | Case of expression * cases

and expression = expression' Span.located
and annotation' = { name : name; typ : expression; implicit : bool }
and annotation = annotation' Span.located
and case' = { pattern : pattern; body : expression }
and case = case' Span.located
and cases = case list Span.located
and pattern' = Wildcard | Variable of name | Constructor of name * patterns
and pattern = pattern' Span.located
and patterns = pattern list Span.located

type telescope' = annotation list
type telescope = telescope' Span.located
type constructor' = { name : name; parameters : telescope; body : expression }
type constructor = constructor' Span.located
type constructors = constructor list Span.located

type define = {
  name : name;
  parameters : telescope;
  body : expression;
  typ : expression;
}

type inductive = {
  name : name;
  parameters : telescope;
  constructors : constructors;
  typ : expression;
}

type command' =
  | Evaluate of expression
  | Define of define
  | Inductive of inductive

type command = command' Span.located
