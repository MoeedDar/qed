type t = Term.t list

let empty = []
let weaken = List.map Substitution.weaken
let add ctx typ = weaken (typ :: ctx)
let get ctx i = List.nth ctx i
