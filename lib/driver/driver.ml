open Core
open Utils

type result = {
  environment : Environment.t;
  global_context : Global_context.t;
  local_context : Local_context.t;
  occurrences : Source.Occurrences.t;
  holes : int list;
  terms : Term_meta_context.t;
  levels : Level_meta_context.t;
  failed_constraints : Core.Constraints.t list;
  postponed_constraints : Core.Constraints.t list;
  diagnostics : Diagnostics.message list;
  crashed : bool;
}

type context = {
  elaboration : Elaboration.t;
  operators : Syntax.Operator.Table.t;
}

let solve (elab : Elaboration.result) =
  Unification.solve elab.terms elab.levels elab.environment elab.constraints

let context_of diags =
  let operators = Syntax.Operator.Table.empty in
  let cmds = Syntax.parse_with operators diags Prelude.text in
  let elaboration = Elaboration.elaborate (Elaboration.create diags) cmds in
  let elab = Elaboration.result elaboration in
  let solution = solve elab in
  let elaboration =
    Elaboration.settle elaboration solution.terms solution.levels
  in
  { elaboration; operators }

let report (solution : Unification.solution) (c : Core.Constraints.t) =
  let instantiate = Term_meta_context.instantiate solution.terms in
  let msg =
    match c.Span.value with
    | Core.Constraints.Conversion (actual, expected) ->
        Diagnostics.Type_mismatch (instantiate actual, instantiate expected)
    | Core.Constraints.Universe_inequality (left, right) ->
        Diagnostics.Universe_mismatch (left, right)
  in
  Some (Span.locate msg c.Span.span)

let kernel_message (error : Kernel.error) =
  match error with
  | Kernel.Mismatch (actual, expected) ->
      Diagnostics.Type_mismatch (actual, expected)
  | Kernel.Expected_sort tm -> Diagnostics.Expected_sort tm
  | Kernel.Expected_pi tm -> Diagnostics.Expected_pi tm
  | Kernel.No_meta -> Diagnostics.Internal_error "kernel: unsolved metavariable"
  | Kernel.No_bad -> Diagnostics.Internal_error "kernel: error term"

let kernel_errors env =
  if Kernel.pending env then []
  else
    match Kernel.check_declarations env with
    | None -> []
    | Some error -> [ Span.locate (kernel_message error) Span.zero ]

let empty =
  {
    environment = Environment.empty;
    global_context = Global_context.empty;
    local_context = Local_context.empty;
    occurrences = Source.Occurrences.create ();
    holes = [];
    terms = Term_meta_context.empty;
    levels = Level_meta_context.empty;
    failed_constraints = [];
    postponed_constraints = [];
    diagnostics = [];
    crashed = true;
  }

let result_of diags ctx source =
  let cmds = Syntax.parse_with ctx.operators diags source in
  let elaboration = Elaboration.elaborate ctx.elaboration cmds in
  let elab = Elaboration.result elaboration in
  let solution = solve elab in
  let elaboration_messages = Diagnostics.diagnostics diags in
  let reported = List.filter_map (report solution) solution.failed in
  let certified =
    match elaboration_messages @ reported with
    | _ :: _ -> []
    | [] -> kernel_errors elab.environment
  in
  {
    environment = elab.environment;
    global_context = elab.global_context;
    local_context = elab.local_context;
    occurrences = elab.occurrences;
    holes = elab.holes;
    terms = solution.terms;
    levels = solution.levels;
    failed_constraints = solution.failed;
    postponed_constraints = solution.postponed;
    diagnostics = elaboration_messages @ reported @ certified;
    crashed = false;
  }

let protect diags ctx source =
  Printexc.record_backtrace true;
  try result_of diags ctx source
  with e ->
    let msg =
      Printf.sprintf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ())
    in
    Diagnostics.report diags (Internal_error msg) Span.zero;
    { empty with diagnostics = Diagnostics.diagnostics diags }

let run_in ctx source =
  let diags = Diagnostics.create () in
  protect diags ctx source

let run source =
  let diags = Diagnostics.create () in
  protect diags (context_of diags) source

let render_context r =
  {
    Pretty.Diagnostics.global_context = r.global_context;
    local_context = r.local_context;
    terms = r.terms;
  }

let message_text r (msg : Diagnostics.message) =
  Pretty.Diagnostics.message (render_context r) msg.value

let messages r = List.map (message_text r) r.diagnostics

let normalise r tm =
  let find_term = Term_meta_context.find_term r.terms in
  let tm = Term_meta_context.instantiate r.terms tm in
  Reduction.normalise find_term r.environment tm

let global_name r id =
  match Global_context.name r.global_context id with
  | Some n -> n
  | None -> "?" ^ string_of_int id

let local_names r =
  let lc = r.local_context in
  fun i -> Local_context.find_name lc i

let pretty r tm =
  let tm = Term_meta_context.instantiate r.terms tm in
  Pretty.Render.term r.global_context r.local_context tm

let pretty_normalised r tm =
  Pretty.Render.term r.global_context r.local_context (normalise r tm)

let value_of r name =
  let* id = Global_context.find r.global_context name in
  let* tm = Environment.get_value r.environment id in
  Some (normalise r tm)

let type_of r name =
  let* id = Global_context.find r.global_context name in
  Environment.get_type r.environment id
