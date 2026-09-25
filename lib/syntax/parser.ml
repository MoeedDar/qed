open Expression
open Span
open Syntax_tree

type t = Diagnostics.t

let report (ctx : t) msg expr = Diagnostics.report ctx msg expr.span
let locate value located = Span.locate value located.span
let bad expr = locate Syntax_tree.Bad expr

let rec unapply expr =
  match expr.value with
  | Expression.Application (func, arg) ->
      let func, args = unapply func in
      let args = arg :: args in
      (func, args)
  | _ -> (expr, [])

let rec unbar expr =
  match expr.value with
  | Bar (left, right) -> unbar left @ unbar right
  | _ -> [ expr ]

let parse_level symbol =
  if String.starts_with ~prefix:"Type" symbol then
    let suffix = String.sub symbol 4 (String.length symbol - 4) in
    if suffix = "" then Some 0 else int_of_string_opt suffix
  else None

let parse_name ctx expr =
  match expr.value with
  | Symbol name -> locate name expr
  | _ ->
      report ctx Expected_name expr;
      locate "" expr

let parse_name_opt ctx expr =
  match expr.value with Expression.Hole -> locate "" expr | _ -> parse_name ctx expr

let rec parse_annotation ?(implicit = false) ctx expr =
  match expr.value with
  | Curly_brace inner -> parse_annotation ~implicit:true ctx inner
  | Colon (name, typ) ->
      let name = parse_name_opt ctx name in
      let typ = parse_expression ctx typ in
      locate { name; implicit; typ } expr
  | _ ->
      report ctx Expected_annotation expr;
      locate { name = locate "" expr; implicit; typ = bad expr } expr

and parse_domain ?(implicit = false) ctx expr =
  match expr.value with
  | Curly_brace inner -> parse_domain ~implicit:true ctx inner
  | Colon (name, typ) ->
      let name = parse_name_opt ctx name in
      let typ = parse_expression ctx typ in
      locate { name; implicit; typ } expr
  | _ ->
      let typ = parse_expression ctx expr in
      locate { name = locate "" expr; implicit; typ } expr

and parse_binding ?(implicit = false) ctx expr =
  match expr.value with
  | Curly_brace inner -> parse_binding ~implicit:true ctx inner
  | Colon (name, typ) ->
      let name = parse_name_opt ctx name in
      let typ = parse_expression ctx typ in
      locate { name; implicit; typ } expr
  | _ ->
      let name = parse_name_opt ctx expr in
      locate { name; implicit; typ = locate Hole expr } expr

and parse_expression ctx expr =
  let parse_expression = parse_expression ctx in
  let value =
    match expr.value with
    | Expression.Bad -> Bad
    | Hole -> Hole
    | Natural n -> Natural n
    | Symbol symbol -> (
        match parse_level symbol with
        | Some level -> Sort level
        | None -> Variable symbol)
    | Arrow (bind, body) ->
        let bind = parse_domain ctx bind in
        let body = parse_expression body in
        Pi (bind, body)
    | Fat_arrow (bind, body) ->
        let bind = parse_binding ctx bind in
        let body = parse_expression body in
        Lambda (bind, body)
  | Application (func, arg) ->
        let func = parse_expression func in
        let arg = parse_expression arg in
        Application (func, arg)
    | Let (bind, value, body) ->
        let bind = parse_binding ctx bind in
        let value = parse_expression value in
        let body = parse_expression body in
        Let (bind, value, body)
    | Case (scrut, cases) ->
        let scrut = parse_expression scrut in
        let cases = parse_cases ctx cases in
        Case (scrut, cases)
    | _ ->
        report ctx Unexpected_expression expr;
        Bad
  in
  locate value expr

and parse_cases ctx expr =
  let branches =
    match expr.value with
    | Bar (lhs, rhs) ->
        let lhs = parse_cases ctx lhs in
        let rhs = parse_cases ctx rhs in
        lhs.value @ rhs.value
    | Fat_arrow (pat, body) ->
        let pattern = parse_pattern ctx pat in
        let body = parse_expression ctx body in
        [ locate { pattern; body } expr ]
    | _ ->
        report ctx Unexpected_expression expr;
        []
  in
  locate branches expr

and parse_pattern_part ctx expr =
  match expr.value with Expression.Hole -> None | _ -> Some (parse_name ctx expr)

and parse_pattern ctx expr =
  let name, args = unapply expr in
  let name = parse_pattern_part ctx name in
  match (name, args) with
  | None, [] -> locate Wildcard expr
  | Some name, [] -> locate (Variable name) expr
  | Some name, args ->
      let args = args |> List.rev |> List.map (parse_pattern ctx) in
      let args = locate args expr in
      locate (Constructor (name, args)) expr
  | None, _ ->
      report ctx Expected_name expr;
      locate Wildcard expr

let parse_head ctx expr =
  let name, annots = unapply expr in
  let name = parse_name ctx name in
  let annots = annots |> List.rev |> List.map (parse_annotation ctx) in
  (name, annots)

let parse_head_infer ctx expr =
  let name, annots = unapply expr in
  let name = parse_name ctx name in
  let annots = annots |> List.rev |> List.map (parse_binding ctx) in
  (name, annots)

let parse_sign ctx expr =
  match expr.value with
  | Colon (head, typ) ->
      let name, params = parse_head ctx head in
      let typ = parse_expression ctx typ in
      (name, params, typ)
  | _ ->
      let name, params = parse_head_infer ctx expr in
      let hole = locate (Hole : expression') expr in
      (name, params, hole)

let parse_ctors ctx expr =
  match expr.value with
  | Colon (head, body) ->
      let name, params = parse_head_infer ctx head in
      let parameters = locate params head in
      let body = parse_expression ctx body in
      Some (locate { name; parameters; body } expr)
  | _ ->
      report ctx Unexpected_expression expr;
      None

let parse_case ctx expr =
  match expr.value with
  | Fat_arrow (pat, body) ->
      let pattern = parse_pattern ctx pat in
      let body = parse_expression ctx body in
      Some (locate { pattern; body } expr)
  | _ ->
      report ctx Unexpected_expression expr;
      None

let parse_def_scrut ctx (params : telescope) : expression =
  match params.value with
  | param :: _ ->
      let scrut = param.value.name in
      locate (Variable scrut.value : expression') scrut
  | [] ->
      report ctx Expected_scrutinee params;
      bad params

let parse_cases ctx expr =
  match unbar expr with
  | sign :: rest -> (
      let name, annots, typ = parse_sign ctx sign in
      let parameters = locate annots sign in
      match rest with
      | { value = Colon _; _ } :: _ ->
          let ctors = List.filter_map (parse_ctors ctx) rest in
          let constructors = locate ctors expr in
          Inductive { name; parameters; constructors; typ }
      | { value = Fat_arrow _; _ } :: _ ->
          let cases = List.filter_map (parse_case ctx) rest in
          let cases = locate cases expr in
          let scrut = parse_def_scrut ctx parameters in
          let body = locate (Case (scrut, cases)) expr in
          Define { name; parameters; body; typ }
      | _ ->
          report ctx Unexpected_declaration expr;
          Evaluate (bad expr))
  | [] ->
      report ctx Unexpected_declaration expr;
      Evaluate (bad expr)

let parse_define ctx expr =
  match expr.value with
  | Assign (left, right) ->
      let name, params, typ = parse_sign ctx left in
      let parameters = locate params left in
      let body = parse_expression ctx right in
      Define { name; parameters; body; typ }
  | _ -> parse_cases ctx expr

let parse_command ctx (cmd : Expression.t Command.t) =
  let expr = cmd.value.value in
  let kind = cmd.value.kind.value in
  let cmd' =
    match kind with
    | Evaluate -> Evaluate (parse_expression ctx expr)
    | Define -> parse_define ctx expr
  in
  locate cmd' cmd

let parse_one = parse_command
