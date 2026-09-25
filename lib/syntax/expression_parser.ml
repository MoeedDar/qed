open Span
open Tree
open Expression

type t = { diagnostics : Diagnostics.t; operators : Operator.Table.t }

let token_name = function
  | Tree.Symbol name -> name
  | Natural n -> string_of_int n
  | Name name -> name
  | Hole -> "_"
  | Assign -> ":="
  | Fat_arrow -> "=>"
  | Arrow -> "->"
  | Bar -> "|"
  | Colon -> ":"
  | Let -> "let"
  | In -> "in"
  | Case -> "case"
  | _ -> assert false

let locate value token = Span.locate value token.span
let bad token = locate Bad token
let report ctx token msg = Diagnostics.report ctx.diagnostics msg token.span

let report_before ctx token =
  report ctx token (Expected_expression_before (token_name token.value))

let report_after ctx token =
  report ctx token (Expected_expression_after (token_name token.value))

let report_marker ctx token marker = report ctx token (Expected_marker marker)
let op ctx token = Operator.Table.lookup ctx.operators token.value
let is_op ctx token = Option.is_some (op ctx token)

let is_marker marker token =
  match token.value with
  | Tree.Assign -> marker = ":="
  | In -> marker = "in"
  | Bar -> marker = "|"
  | Symbol name -> marker = name
  | _ -> false

let ignore_bar = function
  | { value = Tree.Bar; _ } :: rest -> rest
  | tokens -> tokens

let symbol token =
  let[@warning "-8"] (Tree.Symbol name) = token.value in
  locate (Symbol name) token

let apply func arg = Span.between func arg (Application (func, arg))

let rec expression ctx stop bp op = function
  | [] ->
      report_after ctx op;
      (bad op, [])
  | token :: _ as rest when stop token ->
      report_after ctx token;
      (bad op, rest)
  | token :: rest ->
      let lhs, rest = nud ctx stop token rest in
      let lhs, rest = args ctx stop lhs rest in
      led ctx stop bp lhs rest

and nud ctx stop tok rest =
  match tok.value with
  | List (Parenthesis, tokens) -> (parse_list ctx tokens, rest)
  | List (Curly_brace, tokens) ->
      let inner = parse_list ctx tokens in
      (locate (Curly_brace inner) tok, rest)
  | Symbol name -> nud_symbol ctx stop name tok rest
  | Name name -> (locate (Symbol name) tok, rest)
  | Natural n -> (locate (Natural n) tok, rest)
  | Hole -> (locate Hole tok, rest)
  | Case -> mixfix ctx stop [ "|" ] tok rest
  | Let -> mixfix ctx stop [ ":="; "in" ] tok rest
  | _ ->
      report_before ctx tok;
      (bad tok, rest)

and nud_symbol ctx stop name token rest =
  match op ctx token with
  | Some (Prefix bp) -> prefix ctx stop bp token rest
  | Some (Mixfix parts) -> mixfix ctx stop parts token rest
  | _ -> (locate (Symbol name) token, rest)

and led ctx stop bp lhs = function
  | token :: rest when not (stop token) -> extend ctx stop bp lhs token rest
  | rest -> (lhs, rest)

and extend ctx stop bp lhs token rest =
  match op ctx token with
  | Some (Postfix bp') when bp' >= bp -> postfix ctx stop bp lhs token rest
  | Some (Infix (lbp, rbp)) when lbp >= bp ->
      infix ctx stop bp rbp lhs token rest
  | _ -> (lhs, token :: rest)

and prefix ctx stop bp op rest =
  let rhs, rest = expression ctx stop bp op rest in
  (apply (symbol op) rhs, rest)

and postfix ctx stop bp lhs op rest =
  led ctx stop bp (apply (symbol op) lhs) rest

and infix ctx stop bp rbp lhs op rest =
  let rest = if op.value = Assign then ignore_bar rest else rest in
  let rhs, rest = expression ctx stop rbp op rest in
  let value = infix_value op lhs rhs in
  led ctx stop bp value rest

and infix_value op lhs rhs =
  (match op.value with
    | Tree.Assign -> Assign (lhs, rhs)
    | Fat_arrow -> Fat_arrow (lhs, rhs)
    | Arrow -> Arrow (lhs, rhs)
    | Bar -> Bar (lhs, rhs)
    | Colon -> Colon (lhs, rhs)
    | Symbol _ -> Application (apply (symbol op) lhs, rhs)
    | _ -> assert false)
  |> Span.between lhs rhs

and mixfix ctx stop parts op rest =
  let values, rest = mixfix_parts ctx stop op [] rest parts in
  (build_mixfix op values, rest)

and build_mixfix op = function
  | [ bind; value; body ] when op.value = Let ->
      Span.between bind body (Let (bind, value, body))
  | [ scrut; cases ] when op.value = Case ->
      Span.between scrut cases (Case (scrut, cases))
  | values -> List.fold_left apply (symbol op) values

and mixfix_parts ctx stop op values rest = function
  | [] ->
      let rest = if op.value = Case then ignore_bar rest else rest in
      let value, rest = expression ctx stop min_int op rest in
      let values = value :: values in
      (List.rev values, rest)
  | marker :: parts ->
      let stop' token = stop token || is_marker marker token in
      let value, rest = expression ctx stop' min_int op rest in
      let values = value :: values in
      require_marker ctx stop op marker values rest parts

and require_marker ctx stop op marker values rest parts =
  match rest with
  | token :: rest when is_marker marker token ->
      mixfix_parts ctx stop op values rest parts
  | token :: rest ->
      report_marker ctx token marker;
      ([ bad token ], rest)
  | [] ->
      report_marker ctx op marker;
      ([ bad op ], [])

and args ctx stop func = function
  | token :: rest when not (stop token || is_op ctx token) ->
      let arg, rest = nud ctx stop token rest in
      let arg, rest = led ctx stop max_int arg rest in
      args ctx stop (apply func arg) rest
  | rest -> (func, rest)

and parse_list ctx = function
  | token :: _ as tokens ->
      let never _ = false in
      fst (expression ctx never min_int token tokens)
  | [] -> assert false

let parse diagnostics operators tree =
  let ctx = { diagnostics; operators } in
  match tree.value with
  | Tree.List (_, tokens) -> parse_list ctx tokens
  | _ -> assert false
