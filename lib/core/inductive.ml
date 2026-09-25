type t = { typ : Term.t; constructors : int list; eliminator_id : int }

let make typ constructors eliminator_id =
  let constructors = List.map fst constructors in
  { typ; constructors; eliminator_id }

let eliminator_id t = t.eliminator_id
