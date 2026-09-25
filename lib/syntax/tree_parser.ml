open Span
open Utils
open Expression_token
open Tree

let report = Diagnostics.report
let make_tree bracket acc span = Span.locate (List (bracket, List.rev acc)) span

let closer = function Parenthesis -> ')' | Curly_brace -> '}'

let matches_open bracket = function
  | Right_parenthesis -> bracket = Some Parenthesis
  | Right_curly_brace -> bracket = Some Curly_brace
  | _ -> false

let dispatch bracket k_left k_right k_closed add = function
  | Expression_token.Symbol symbol -> add (Symbol symbol)
  | Natural n -> add (Natural n)
  | Hole -> add Hole
  | Assign -> add Assign
  | Fat_arrow -> add Fat_arrow
  | Arrow -> add Arrow
  | Bar -> add Bar
  | Colon -> add Colon
  | Let -> add Let
  | In -> add In
  | Case -> add Case
  | Left_parenthesis -> k_left Parenthesis ()
  | Left_curly_brace -> k_left Curly_brace ()
  | (Right_parenthesis | Right_curly_brace) as close ->
      if matches_open bracket close then k_right () else k_closed close

let rec parse ctx bracket start k_eof k_right acc = function
  | token :: rest ->
      let span = token.span in
      let go (tree, rest) =
        parse ctx bracket start k_eof k_right (tree :: acc) rest
      in
      let add value = go (Span.locate value span, rest) in
      let left bracket () = go (parse_list bracket ctx span rest) in
      let right () = k_right acc span rest in
      let closed _close =
        report ctx
          (match bracket with
          | Some kind -> Mismatched_bracket (closer kind)
          | None -> Unexpected_closing_bracket)
          span;
        parse ctx bracket start k_eof k_right acc rest
      in
      dispatch bracket left right closed add token.value
  | [] ->
      let bracket =
        match bracket with Some bracket -> bracket | None -> Tree.Parenthesis
      in
      (make_tree bracket acc start, []) |> k_eof

and parse_list bracket ctx start tokens =
  let k_eof _ = report ctx Expected_closing_bracket start in
  let k_right acc span rest =
    let span = Span.merge start span in
    (make_tree bracket acc span, rest)
  in
  parse ctx (Some bracket) start (tap k_eof) k_right [] tokens

let rec parse_top_level ctx acc tokens =
  let k_right acc span rest =
    report ctx Unexpected_closing_bracket span;
    parse_top_level ctx acc rest
  in
  parse ctx None Span.zero Fun.id k_right acc tokens

let parse diags tokens = parse_top_level diags [] tokens |> fst
