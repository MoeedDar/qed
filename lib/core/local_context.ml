type t = { names : string list; types : Context.t }

let empty = { names = []; types = Context.empty }

let add t name typ =
  { names = name :: t.names; types = Context.add t.types typ }

let get_type t i = Context.get t.types i
let get_name t i = List.nth t.names i
let get t i = (get_name t i, get_type t i)
let find_name t i = List.nth_opt t.names i
let find_index t name = List.find_index (( = ) name) t.names
let find t i = find_name t i |> Option.map (fun nm -> (nm, get_type t i))
