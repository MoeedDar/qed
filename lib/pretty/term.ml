open Core.Term
open Core.Level

let rec level_count = function
  | Zero -> Some 0
  | Succ l -> Option.map (( + ) 1) (level_count l)
  | _ -> None

let level l =
  match level_count l with
  | None -> "Type"
  | Some 0 -> "Type"
  | Some n -> "Type" ^ string_of_int n

let letters = [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" |]

let named n =
  if n < Array.length letters then letters.(n)
  else "x" ^ string_of_int (n - Array.length letters)

let rec fresh names n =
  if List.mem (named n) names then fresh names (n + 1) else named n

let global_name gc id =
  match Core.Global_context.name gc id with
  | Some n -> n
  | None -> "?" ^ string_of_int id

let rec term prec gc names tm =
  match tm with
  | Bad -> "?bad"
  | Meta_variable i -> "?" ^ string_of_int i
  | Sort l -> level l
  | Variable i -> variable names i
  | Constant (id, _) -> global_name gc id
  | Pi (a, b) ->
      let a = atom gc names a in
      let names = fresh names 0 :: names in
      let b = term 0 gc names b in
      parenthesise (prec > 0) (Printf.sprintf "%s -> %s" a b)
  | Lambda (_, b) ->
      let binder = fresh names 0 in
      let b = term 0 gc (binder :: names) b in
      parenthesise (prec > 0) (Printf.sprintf "%s => %s" binder b)
  | Application (f, x) ->
      let f = term 1 gc names f in
      let x = atom gc names x in
      parenthesise (prec > 1) (Printf.sprintf "%s %s" f x)

and atom global_name names tm =
  match tm with
  | Variable _ | Constant _ | Meta_variable _ | Sort _ | Bad ->
      term 0 global_name names tm
  | _ -> term 10 global_name names tm

and variable names i =
  match List.nth_opt names i with
  | Some n -> n
  | None -> "?var" ^ string_of_int i

and parenthesise cond s = if cond then "(" ^ s ^ ")" else s

let term gc names tm = term 0 gc names tm
