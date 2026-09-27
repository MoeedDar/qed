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
  let str = Driver.pretty_in r lc ty in
  "```qed\n" ^ str ^ "\n```"

let hover_sort level =
  let level = Level.succ_n level Level.Zero in
  "```qed\n" ^ Pretty.Term.level level ^ "\n```"

let hover_declaration (r : Driver.result) names id =
  let name = Pretty.Term.global_name r.global_context id in
  let signature =
    match Environment.find_type r.environment id with
    | Some typ -> name ^ " : " ^ Driver.pretty_named r names typ
    | None -> name
  in
  let value =
    match Environment.find_value r.environment id with
    | Some tm -> "\n  := " ^ Driver.pretty_named r names (Driver.normalise r tm)
    | None -> ""
  in
  "```qed\n" ^ signature ^ value ^ "\n```"

let hover_payload (r : Driver.result) = function
  | Occurrences.Variable { typ; local_context } ->
      hover_variable r local_context typ
  | Occurrences.Sort level -> hover_sort level
  | Occurrences.Declaration { id; names } -> hover_declaration r names id

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
  Driver.pretty_in r local (goal_term r tmc typ)

let goal_line (r : Driver.result) tmc local typ =
  let tm = goal_term r tmc typ in
  let body = Driver.pretty_in r local tm in
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

let open_goals tmc environment =
  let live = Term_meta_context.live tmc (Environment.terms environment) in
  let step id entry acc =
    if List.mem_assoc id live then entry :: acc else acc
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
    let entries = open_goals tmc r.environment |> order_goals in
    List.map (goal_of_entry r tmc document) entries
