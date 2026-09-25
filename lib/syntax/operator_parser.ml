open Span
open Utils

let symbol = function { value = Tree.Symbol name; _ } -> Some name | _ -> None

let natural = function
  | { value = Tree.Natural prec; _ } -> Some prec
  | _ -> None

let single parser = function [ token ] -> parser token | _ -> None
let single_symbol = single symbol

let infix = function
  | [ lhs; op; rhs ] ->
      let* lhs = natural lhs in
      let* op = symbol op in
      let* rhs = natural rhs in
      Some (op, Operator.Infix (lhs, rhs))
  | _ -> None

let postfix = function
  | [ prec; op ] ->
      let* prec = natural prec in
      let* op = symbol op in
      Some (op, Operator.Postfix prec)
  | _ -> None

let prefix = function
  | [ op; prec ] ->
      let* op = symbol op in
      let* prec = natural prec in
      Some (op, Operator.Prefix prec)
  | _ -> None

let split_holes tokens =
  let rec loop current segments = function
    | [] -> List.rev (List.rev current :: segments)
    | { value = Tree.Hole; _ } :: tokens ->
        loop [] (List.rev current :: segments) tokens
    | token :: tokens -> loop (token :: current) segments tokens
  in

  loop [] [] tokens

let rec parse_parts = function
  | [] -> Some []
  | part :: parts ->
      let* part = single_symbol part in
      let* parts = parse_parts parts in
      Some (part :: parts)

let drop_trailing parts =
  match List.rev parts with [] :: rest -> List.rev rest | _ -> parts

let mixfix tokens =
  match split_holes tokens with
  | [] -> None
  | head :: parts ->
      let* name = single_symbol head in
      let* parts = parse_parts (drop_trailing parts) in
      Some (name, Operator.Mixfix parts)

let parse_op tokens =
  List.find_map (fun parser -> parser tokens) [ infix; postfix; prefix; mixfix ]

let rw_head name head = { head with value = Tree.Name name }

let rw_cmd bracket name head rest =
  let head = rw_head name head in
  Tree.List (bracket, head :: rest)

let def_op ops name op = Operator.Table.add ops name op
let bad_op diags head = Diagnostics.report diags Invalid_operator head.span

let parse_def diags ops tree =
  match tree.value with
  | Tree.List
      ( bracket,
        ({ value = Tree.List (Parenthesis, skeleton); _ } as head) :: rest )
    -> (
      match parse_op skeleton with
      | None ->
          bad_op diags head;
          tree
      | Some (name, operator) ->
          def_op ops name operator;
          let value = rw_cmd bracket name head rest in
          { tree with value })
  | _ -> tree

let parse diags ops (cmd : 'a Command.t) tree =
  match cmd.value.kind.value with
  | Command.Evaluate -> tree
  | Command.Define -> parse_def diags ops tree
