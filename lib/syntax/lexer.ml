type t = {
  source : string;
  length : int;
  mutable position : int;
  mutable tokens : Token.t list;
}

let tap = Utils.tap

let create source =
  { source; length = String.length source; position = 0; tokens = [] }

let span st start = Span.make start st.position
let token st start kind = span st start |> Span.locate kind
let append st token = st.tokens <- token :: st.tokens
let emit st kind start = token st start kind |> append st
let flush st = List.rev st.tokens
let slice st start = span st start |> Span.slice st.source

let peek st =
  if st.position >= st.length then '\000' else st.source.[st.position]

let bump st _ = st.position <- st.position + 1
let bump st x = tap (bump st) x
let take st = peek st |> bump st
let eat st c = peek st = c && bump st true

let rec take_while st start pred =
  let go _ = take_while st start pred in
  match peek st with c when pred c -> go (bump st c) | _ -> slice st start

let rec take_until st start pred =
  let go _ = take_until st start pred in
  match peek st with c when pred c -> () | _ -> go (bump st ())

let is_newline = function '\n' | '\r' -> true | _ -> false
let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
let is_delimiter = function '(' | ')' | '{' | '}' -> true | _ -> false
let is_symbol c = not (is_whitespace c || is_delimiter c || c = '\000')
let is_digit c = c >= '0' && c <= '9'

let lex src =
  let st = create src in
  let eat = eat st in
  let rec go () =
    let start = st.position in
    let emit kind = emit st kind start |> go in
    let take_while = take_while st start in
    let take_until = take_until st start in
    match take st with
    | '\000' -> flush st
    | c when is_whitespace c -> go ()
    | ':' when eat '=' -> emit Assign
    | '=' when eat '>' -> emit Fat_arrow
    | '-' when eat '>' -> emit Arrow
    | '-' when eat '-' -> take_until is_newline |> go
    | '_' -> emit Hole
    | '|' -> emit Bar
    | ':' -> emit Colon
    | '(' -> emit Left_parenthesis
    | ')' -> emit Right_parenthesis
    | '{' -> emit Left_curly_brace
    | '}' -> emit Right_curly_brace
    | '0' .. '9' -> take_while is_digit |> Token.of_digit |> emit
    | _ -> take_while is_symbol |> Token.of_text |> emit
  in
  go ()
