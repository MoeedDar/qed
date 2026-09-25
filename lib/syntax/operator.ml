open Tree

type t =
  | Prefix of int
  | Postfix of int
  | Infix of int * int
  | Mixfix of string list

module Table = struct
  type nonrec t = (string, t) Hashtbl.t

  let empty : t = Hashtbl.create 8
  let add (t : t) name op = Hashtbl.replace t name op

  let lookup ops = function
    | Symbol name -> Hashtbl.find_opt ops name
    | Assign -> Some (Infix (-9, -10))
    | Fat_arrow -> Some (Infix (-5, -6))
    | Bar -> Some (Infix (-7, -8))
    | Colon -> Some (Infix (-3, -4))
    | Arrow -> Some (Infix (-1, -2))
    | Let -> Some (Mixfix [ ":="; "in" ])
    | Case -> Some (Mixfix [ "|" ])
    | Hole | In | List _ | Natural _ | Name _ -> None
end
