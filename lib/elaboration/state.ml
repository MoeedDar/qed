open Core
open Utils

type self = { name : string; term : Term.t; typ : Term.t }

type t = {
  diagnostics : Diagnostics.t;
  occurrences : Source.Occurrences.t;
  environment : Environment.t;
  local_context : Core.Local_context.t;
  global_context : Global_context.t;
  term_meta_context : Term_meta_context.t;
  level_meta_context : Level_meta_context.t;
  constraints : Core.Constraints.t list;
  implicits : bool list Id_map.t;
  namespace : string;
  self : self option;
  holes : int list;
}

let create diagnostics =
  {
    diagnostics;
    occurrences = Source.Occurrences.create ();
    environment = Environment.empty;
    local_context = Core.Local_context.empty;
    global_context = Global_context.empty;
    term_meta_context = Term_meta_context.empty;
    level_meta_context = Level_meta_context.empty;
    constraints = [];
    implicits = Id_map.empty;
    namespace = "";
    self = None;
    holes = [];
  }

let report st msg span = Diagnostics.report st.diagnostics msg span

let with_environment st environment = { st with environment }
let with_local_context st local_context = { st with local_context }
let with_global_context st global_context = { st with global_context }

let with_term_meta_context st term_meta_context =
  { st with term_meta_context }

let with_level_meta_context st level_meta_context =
  { st with level_meta_context }

let with_constraints st constraints = { st with constraints }
let with_implicits st implicits = { st with implicits }
let with_namespace st namespace = { st with namespace }
let with_self st self = { st with self }
let with_holes st holes = { st with holes }

let foldi st f xs =
  let rec go st acc i = function
    | x :: xs ->
        let st, y = f st i x in
        go st (y :: acc) (i + 1) xs
    | [] -> (st, List.rev acc)
  in
  go st [] 0 xs

let fold st f xs = foldi st (fun st _ x -> f st x) xs
