module Map = Map.Make (Int)

type 'a t = { id : int; map : 'a Map.t }

let empty = { id = -1; map = Map.empty }
let next map = map.id + 1
let with_map map map' = { map with map = map' }
let with_next map = { map with id = next map }
let map_add map key value = with_map map (Map.add key value map.map)
let mem map id = Map.mem id map.map
let add map value = map_add (with_next map) (next map) value
let set map id value = if mem map id then map_add map id value else map
let insert map id value = map_add map id value
let find map id = Map.find_opt id map.map
let fold map f = Map.fold f map.map
