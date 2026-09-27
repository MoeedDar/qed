open Core
open Utils

type result = {
  diagnostics : Diagnostics.message list;
  occurrences : Source.Occurrences.t;
  environment : Environment.t;
  global_context : Global_context.t;
  local_context : Local_context.t;
  term_meta_context : Term_meta_context.t;
  local_meta_context : Level_meta_context.t;
  failed_constraints : Constraints.t list;
  postponed_constraints : Constraints.t list;
  holes : int list;
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
  let detail =
    match error with
    | Kernel.Mismatch _ -> "type mismatch"
    | Kernel.Expected_sort _ -> "expected a type"
    | Kernel.Expected_pi _ -> "expected a function type"
    | Kernel.No_meta -> "unsolved metavariable"
    | Kernel.No_bad -> "error term"
  in
  Diagnostics.Internal_error ("kernel: " ^ detail)

let check_one env _ declaration found =
  match (found, Declaration.define_of declaration) with
  | Error _, _ | Ok (), None -> found
  | Ok (), Some d -> Kernel.check env Core.Context.empty d.term d.typ

let kernel_check env = Id_map.fold env (check_one env) (Ok ())

let kernel_errors env =
  match kernel_check env with
  | Ok () -> []
  | Error error -> [ Span.locate (kernel_message error) Span.zero ]

let unverified env terms = Term_meta_context.live terms (Environment.terms env)
let verified env terms = unverified env terms = []

let empty =
  {
    environment = Environment.empty;
    global_context = Global_context.empty;
    local_context = Local_context.empty;
    occurrences = Source.Occurrences.create ();
    holes = [];
    term_meta_context = Term_meta_context.empty;
    local_meta_context = Level_meta_context.empty;
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
  let instantiate = Term_meta_context.instantiate solution.terms in
  let environment = Environment.instantiate instantiate elab.environment in
  let elaboration_messages = Diagnostics.diagnostics diags in
  let reported = List.filter_map (report solution) solution.failed in
  let certified =
    match elaboration_messages @ reported with
    | _ :: _ -> []
    | [] when not (verified environment solution.terms) -> []
    | [] -> kernel_errors environment
  in
  {
    environment;
    global_context = elab.global_context;
    local_context = elab.local_context;
    occurrences = elab.occurrences;
    holes = elab.holes;
    term_meta_context = solution.terms;
    local_meta_context = solution.levels;
    failed_constraints = solution.failed;
    postponed_constraints = solution.postponed;
    diagnostics = elaboration_messages @ reported @ certified;
    crashed = false;
  }

let protect diags ctx src =
  Printexc.record_backtrace true;
  try result_of diags ctx src
  with e ->
    let msg =
      Printf.sprintf "%s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ())
    in
    Diagnostics.report diags (Internal_error msg) Span.zero;
    { empty with diagnostics = Diagnostics.diagnostics diags }

let run_in ctx src =
  let diags = Diagnostics.create () in
  protect diags ctx src

let run src =
  let diags = Diagnostics.create () in
  protect diags (context_of diags) src

let render_context r =
  {
    Pretty.Diagnostics.global_context = r.global_context;
    local_context = r.local_context;
    terms = r.term_meta_context;
  }

let message_text r (msg : Diagnostics.message) =
  Pretty.Diagnostics.message (render_context r) msg.value

let messages r = List.map (message_text r) r.diagnostics

let normalise r tm =
  let find_term = Term_meta_context.find_term r.term_meta_context in
  let tm = Term_meta_context.instantiate r.term_meta_context tm in
  Reduction.normalise find_term r.environment tm

let global_name r id =
  match Global_context.name r.global_context id with
  | Some n -> n
  | None -> "?" ^ string_of_int id

let pretty_in r lc tm =
  let tm = Term_meta_context.instantiate r.term_meta_context tm in
  Pretty.Term.term r.global_context lc.Local_context.names tm

let pretty_named r names tm =
  let tm = Term_meta_context.instantiate r.term_meta_context tm in
  Pretty.Term.term r.global_context names tm

let pretty r tm = pretty_in r r.local_context tm
let pretty_normalised r tm = pretty_in r r.local_context (normalise r tm)
let pretty_closed r tm = pretty_in r Local_context.empty tm

let value_of r nm =
  let* id = Global_context.find r.global_context nm in
  let* tm = Environment.find_value r.environment id in
  Some (normalise r tm)

let type_of r nm =
  let* id = Global_context.find r.global_context nm in
  Environment.find_type r.environment id
