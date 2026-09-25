type t =
  | Zero
  | Meta_variable of int
  | Variable of int
  | Succ of t
  | Max of t * t

let succ l = Succ l
let rec succ_n n l = if n <= 0 then l else succ l |> succ_n (n - 1)
let join a b = Max (a, b)
let join_all = List.fold_left join Zero

module Reduction = struct
  type term = Variable of int | Meta_variable of int

  module Polynomial = Map.Make (struct
    type t = term

    let compare a b =
      match (a, b) with
      | Variable a, Variable b -> Int.compare a b
      | Variable _, Meta_variable _ -> -1
      | Meta_variable _, Variable _ -> 1
      | Meta_variable a, Meta_variable b -> Int.compare a b
  end)

  type polynomial = int * int Polynomial.t

  let to_polynomial l =
    let add_tm tms tm d =
      let depth = function None -> Some d | Some d' -> Some (max d d') in
      Polynomial.update tm depth tms
    in

    let rec go d k tms = function
      | Zero -> (max k d, tms)
      | Meta_variable i -> (k, add_tm tms (Meta_variable i) d)
      | Variable i -> (k, add_tm tms (Variable i) d)
      | Succ l -> go (d + 1) k tms l
      | Max (a, b) ->
          let k, tms = go d k tms a in
          go d k tms b
    in

    let k, tms = go 0 0 Polynomial.empty l in
    let max = Polynomial.fold (fun _ d acc -> max acc d) tms (-1) in
    let k = if k <= max then -1 else k in

    (k, tms)

  let of_poly (k, tms) =
    let max a b = Max (a, b) in
    let level_of_tm n = function
      | Variable i -> succ_n n (Variable i)
      | Meta_variable i -> succ_n n (Meta_variable i)
    in
    let fold_tms tm d acc = level_of_tm d tm :: acc in
    let lvls = Polynomial.fold fold_tms tms [] in
    let lvls = if k >= 0 then succ_n k Zero :: lvls else lvls in
    match List.rev lvls with [] -> Zero | l :: ls -> List.fold_left max l ls

  let normalise l = to_polynomial l |> of_poly
end

module Relation = struct
  let equals l l' =
    l = l'
    ||
    let k, tms = Reduction.to_polynomial l in
    let k', tms' = Reduction.to_polynomial l' in
    k = k' && Reduction.Polynomial.equal ( = ) tms tms'

  let all_equals ls ls' =
    List.length ls = List.length ls' && (ls = ls' || List.for_all2 equals ls ls')
end

module Meta_variable = struct
  let rec occurs m = function
    | Meta_variable n -> n = m
    | Succ l -> occurs m l
    | Max (l, l') -> occurs m l || occurs m l'
    | _ -> false

  let rec contains = function
    | Meta_variable _ -> true
    | Succ l -> contains l
    | Max (l, l') -> contains l || contains l'
    | _ -> false
end
