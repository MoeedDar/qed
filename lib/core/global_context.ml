module Map = Map.Make (String)

type t = int Map.t

let empty = Map.empty
let add t name id = Map.add name id t
let find t name = Map.find_opt name t

let name t id =
  Map.fold (fun n i acc -> if i = id && acc = None then Some n else acc) t None
