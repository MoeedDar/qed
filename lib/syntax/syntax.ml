include Syntax_tree

(* Operator declarations are threaded between parses, so a prelude's operators
   survive into a later, separate parse. *)
module Operator = Operator

(* [table] is filled in as the source is parsed. *)
let parse_with table diags src =
  let parse_command cmd =
    Command.value cmd
    |> Tree_parser.parse diags
    |> Operator_parser.parse diags table cmd
    |> Expression_parser.parse diags table
    |> Command.map cmd
    |> Parser.parse_one diags
  in

  Lexer.lex src |> Command_parser.filter |> List.map parse_command

let parse diags src = parse_with Operator.Table.empty diags src
