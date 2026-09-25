open Span

type t = {
  kind : Command.kind';
  start : Span.t;
  tokens : Expression_token.t list;
}

let initial = { kind = Evaluate; start = Span.zero; tokens = [] }
let make kind start = { kind; start; tokens = [] }
let add st token = { st with tokens = token :: st.tokens }

let emit st result token =
  let span = Span.merge st.start token.span in
  let kind = Span.locate st.kind span in
  let toks = List.rev st.tokens in
  let cmd = Command.make kind toks in
  Span.locate cmd span :: result

let finish st result =
  match st.tokens with token :: _ -> emit st result token | [] -> result

let action k_cmd k_expr = function
  | Token.Def -> k_cmd Command.Define
  | Symbol s -> k_expr (Expression_token.Symbol s)
  | Natural n -> k_expr (Natural n)
  | Hole -> k_expr Hole
  | Left_parenthesis -> k_expr Left_parenthesis
  | Right_parenthesis -> k_expr Right_parenthesis
  | Left_curly_brace -> k_expr Left_curly_brace
  | Right_curly_brace -> k_expr Right_curly_brace
  | Assign -> k_expr Assign
  | Fat_arrow -> k_expr Fat_arrow
  | Arrow -> k_expr Arrow
  | Bar -> k_expr Bar
  | Colon -> k_expr Colon
  | Let -> k_expr Let
  | In -> k_expr In
  | Case -> k_expr Case

let step (st, result) token =
  let span = token.span in
  let k_cmd kind = (make kind span, finish st result) in
  let k_expr value = (add st (Span.locate value span), result) in
  action k_cmd k_expr token.value

let rec go (st, acc) = function
  | token :: rest -> go (step (st, acc) token) rest
  | [] -> List.rev (finish st acc)

let filter tokens = go (initial, []) tokens
