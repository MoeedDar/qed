type t =
  | Define of Define.t
  | Inductive of Inductive.t
  | Constructor of Constructor.t
  | Eliminator of Eliminator.t

let type_of = function
  | Define d -> d.typ
  | Inductive i -> i.typ
  | Constructor c -> c.typ
  | Eliminator e -> e.typ

let define d = Define d
let inductive i = Inductive i
let constructor c = Constructor c
let eliminator e = Eliminator e
let make_define params term typ = Define.make params term typ |> define

let make_inductive typ ctors elim_id =
  Inductive.make typ ctors elim_id |> inductive

let make_constructor ind pos ind_params params body =
  Constructor.make ind pos ind_params params body |> constructor

let make_elim ind params fam ctors lvls =
  Eliminator.make ind params fam ctors lvls |> eliminator

let value_of = function Define d -> Some d.term | _ -> None
let inductive_of = function Inductive i -> Some i | _ -> None
let constructor_of = function Constructor c -> Some c | _ -> None
let eliminator_of = function Eliminator e -> Some e | _ -> None
