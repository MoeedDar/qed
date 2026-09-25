type t =
  | Bad
  | Variable of int
  | Meta_variable of int
  | Sort of Level.t
  | Constant of int * Level.t list
  | Pi of t * t
  | Lambda of t * t
  | Application of t * t

let variable i = Variable i
let meta_variable m = Meta_variable m
let sort l = Sort l
let constant i l = Constant (i, l)
let pi a b = Pi (a, b)
let lambda a b = Lambda (a, b)
let application a b = Application (a, b)

let map f = function
  | Pi (a, b) -> pi (f a) (f b)
  | Lambda (a, b) -> lambda (f a) (f b)
  | Application (a, b) -> application (f a) (f b)
  | tm -> tm

module Relation = struct
  let lift f = function
    | Bad, _ | _, Bad -> true
    | Sort l, Sort l' -> Level.Relation.equals l l'
    | Variable i, Variable i' -> i = i'
    | Meta_variable i, Meta_variable i' -> i = i'
    | Constant (i, l), Constant (i', l') ->
        i = i' && Level.Relation.all_equals l l'
    | Pi (a, b), Pi (a', b') -> f a a' && f b b'
    | Lambda (a, b), Lambda (a', b') -> f a a' && f b b'
    | Application (a, b), Application (a', b') -> f a a' && f b b'
    | _ -> false

  let rec equals a b = lift equals (a, b)
end

module Binding = struct
  let rec bind f d = function
    | Variable i -> f d i
    | Pi (a, b) -> pi (bind f d a) (bind f (d + 1) b)
    | Lambda (a, b) -> lambda (bind f d a) (bind f (d + 1) b)
    | Application (a, b) -> application (bind f d a) (bind f d b)
    | term -> term

  let map f d tm =
    let f depth i = f depth i |> variable in
    bind f d tm
end

module Meta_variable = struct
  let bind f = function Meta_variable i -> f i | _ -> None

  let rec occurs m = function
    | Meta_variable n -> n = m
    | Pi (a, b) | Lambda (a, b) | Application (a, b) -> occurs m a || occurs m b
    | _ -> false

  let rec contains = function
    | Meta_variable _ -> true
    | Pi (a, b) | Lambda (a, b) | Application (a, b) -> contains a || contains b
    | _ -> false

  let rec contains_bad = function
    | Bad -> true
    | Pi (a, b) | Lambda (a, b) | Application (a, b) ->
        contains_bad a || contains_bad b
    | _ -> false
end

module Constant = struct
  let bind f = function Constant (i, _) -> f i | _ -> None

  let rec head = function
    | Constant (i, _) -> Some i
    | Application (f, _) -> head f
    | _ -> None

  let id = function Constant (i, _) -> Some i | _ -> None

  let rec occurs i = function
    | Constant (i', _) -> i = i'
    | Pi (a, b) | Lambda (a, b) | Application (a, b) -> occurs i a || occurs i b
    | _ -> false
end

module Pi = struct
  let fold = List.fold_right pi
  let map f = function Pi (a, b) -> Some (pi (f a) (f b)) | _ -> None
  let rec codomain = function Pi (_, b) -> codomain b | tm -> tm
  let rec domains = function Pi (a, b) -> a :: domains b | _ -> []
  let rec codomains = function Pi (_, b) -> codomains b | tm -> tm
  let rec arity = function Pi (_, b) -> 1 + arity b | _ -> 0

  let split n pi =
    let rec go n doms typ =
      if n > 0 then
        match typ with
        | Pi (a, b) -> go (n - 1) (a :: doms) b
        | _ -> invalid_arg "Term.Pi.split: too few domains"
      else (List.rev doms, typ)
    in
    go n [] pi

  let rec append_domain pi dom cod =
    match pi with
    | Pi (dom, cod') -> Pi (dom, append_domain cod' dom cod)
    | _ -> Pi (dom, cod)
end

module Application = struct
  let fold = List.fold_left application

  let unfold term =
    let rec go tm args =
      match tm with Application (f, x) -> go f (x :: args) | tm -> (tm, args)
    in
    go term []

  let arguments tm = unfold tm |> snd

  let map_lambda f = function
    | Application (Lambda (_, b), a) -> Some (f b a)
    | _ -> None

  let of_constant i l args = fold (constant i l) args
end

module Lambda = struct
  let fold = List.fold_right lambda
end
