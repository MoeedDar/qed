module Id_map = Id_map

let ( let* ) = Option.bind
let ( <|> ) f g x = match f x with Some _ as y -> y | None -> g x

let tap f x =
  f x;
  x

let compose g f x = f x |> g
let compose2 g f a b = f a b |> g
let compose3 g f a b c = f a b c |> g
let compose4 g f a b c d = f a b c d |> g
let compose5 g f a b c d e = f a b c d e |> g
let rec last = function [ x ] -> x | _ :: xs -> last xs | [] -> assert false

let rec split_last = function
  | [] -> None
  | [ x ] -> Some (x, [])
  | x :: xs ->
      let* last, rest = split_last xs in
      Some (last, x :: rest)

let rec drop n = function _ :: rest when n > 0 -> drop (n - 1) rest | xs -> xs

let take count list =
  let rec go acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> go (x :: acc) (n - 1) xs
  in
  go [] count list

let rec repeat f x = match f x with Some x -> repeat f x | None -> x
