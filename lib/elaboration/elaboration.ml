type t = State.t

type result = {
  environment : Core.Environment.t;
  global_context : Core.Global_context.t;
  local_context : Core.Local_context.t;
  occurrences : Source.Occurrences.t;
  terms : Core.Term_meta_context.t;
  levels : Core.Level_meta_context.t;
  constraints : Core.Constraints.t list;
  holes : int list;
}

let create diags = State.create diags
let elaborate st cmds = Command.elaborate_all st cmds

let result (t : t) : result =
  {
    environment = t.environment;
    global_context = t.global_context;
    local_context = t.local_context;
    occurrences = t.occurrences;
    terms = t.term_meta_context;
    levels = t.level_meta_context;
    constraints = t.constraints;
    holes = t.holes;
  }

let settle (st : t) terms levels =
  let st = State.with_term_meta_context st terms in
  let st = State.with_level_meta_context st levels in
  let st = State.with_constraints st [] in
  State.with_holes st []
