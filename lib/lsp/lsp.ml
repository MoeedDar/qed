open Core

type position = Span.position

type diagnostic = {
  start : position;
  end_ : position;
  severity : int;
  message : string;
}

type hypothesis = { name : string; typ : string }

type goal = {
  start : position;
  end_ : position;
  context : hypothesis list;
  goal : string;
}

let convert r document (msg : Diagnostics.message) =
  let start = Span.position_of_offset document msg.span.start in
  let end_ = Span.position_of_offset document msg.span.stop in
  let context = Driver.render_context r in
  let message = Pretty.Diagnostics.message context msg.value in
  let severity = Pretty.Diagnostics.severity msg.value in
  { start; end_; severity; message }

let check source =
  let document = Span.document source in
  let r = Driver.run source in
  List.map (convert r document) r.diagnostics

let hover_variable (r : Driver.result) lc ty =
  let str = Pretty.Render.term r.global_context lc ty in
  "```qed\n" ^ str ^ "\n```"

let hover_sort level = "```qed\nType" ^ string_of_int level ^ "\n```"

let hover_declaration (r : Driver.result) id =
  "```qed\n" ^ Pretty.Render.global_name r.global_context id ^ "\n```"

let hover_payload (r : Driver.result) = function
  | Occurrences.Variable { typ; local_context } ->
      hover_variable r local_context typ
  | Occurrences.Sort level -> hover_sort level
  | Occurrences.Declaration id -> hover_declaration r id

let hover_at (r : Driver.result) offset =
  match Occurrences.find r.occurrences offset with
  | None -> None
  | Some occ -> Some (hover_payload r occ.value)

let hover source line col =
  let document = Span.document source in
  let r = Driver.run source in
  if r.crashed then None else hover_at r (Span.offset_of document line col)

let goal_term (r : Driver.result) tmc typ =
  let find_term = Term_meta_context.find_term tmc in
  let tm = Term_meta_context.instantiate tmc typ in
  Reduction.whnf find_term r.environment tm

let goal_body (r : Driver.result) tmc local typ =
  Pretty.Render.term r.global_context local (goal_term r tmc typ)

let goal_line (r : Driver.result) tmc local typ =
  let tm = goal_term r tmc typ in
  let body = Pretty.Render.term r.global_context local tm in
  match tm with
  | Term.Meta_variable _ -> body ^ " (type unresolved)"
  | _ -> body

let goal_context (r : Driver.result) tmc local =
  let rec go i acc =
    match Local_context.find_name local i with
    | None -> List.rev acc
    | Some name ->
        let typ = Local_context.get_type local i in
        let typ = goal_body r tmc local typ in
        go (i + 1) ({ name; typ } :: acc)
  in
  go 0 []

let open_goals tmc holes =
  let step id entry acc =
    let is_goal = List.mem id holes in
    match entry.Term_meta_context.term with
    | Some _ -> acc
    | None when is_goal ->
        let typ = entry.Term_meta_context.typ in
        if Term.Meta_variable.contains_bad typ then acc else entry :: acc
    | _ -> acc
  in
  Utils.Id_map.fold tmc step []

let order_goals entries =
  let by_span a b =
    Stdlib.compare a.Term_meta_context.span.Span.start
      b.Term_meta_context.span.Span.start
  in
  List.sort by_span entries

let goal_of_entry (r : Driver.result) tmc document entry =
  let span = entry.Term_meta_context.span in
  let start = Span.position_of_offset document span.start in
  let end_ = Span.position_of_offset document span.stop in
  let local = entry.Term_meta_context.local in
  let context = goal_context r tmc local in
  let goal = goal_line r tmc local entry.Term_meta_context.typ in
  { start; end_; context; goal }

let goals source =
  let document = Span.document source in
  let r = Driver.run source in
  if r.crashed then []
  else
    let tmc = r.terms in
    let entries = open_goals tmc r.holes |> order_goals in
    List.map (goal_of_entry r tmc document) entries
