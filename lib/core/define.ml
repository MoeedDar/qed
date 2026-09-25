type t = { term : Term.t; typ : Term.t }

let make parameters term typ =
  let term = Term.Lambda.fold parameters term in
  let typ = Term.Pi.fold parameters typ in
  { term; typ }
