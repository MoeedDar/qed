open Core.Term
open Core.Level

let rec level_count = function
  | Zero -> Some 0
  | Succ l -> Option.map (fun n -> n + 1) (level_count l)
  | _ -> None

let level l =
  match level_count l with
  | Some 0 -> "Type"
  | Some n -> "Type" ^ string_of_int n
  | None -> "Type"

let rec term prec globals locals bound tm =
  match tm with
  | Bad -> "?bad"
  | Meta_variable i -> "?" ^ string_of_int i
  | Sort l -> level l
  | Variable i -> variable locals bound i
  | Constant (id, _) -> globals id
  | Pi (a, b) ->
      let a = atom globals locals bound a in
      let b = term 0 globals locals (bound + 1) b in
      parenthesise (prec > 0) (Printf.sprintf "%s -> %s" a b)
  | Lambda (_, b) ->
      let b = term 0 globals locals (bound + 1) b in
      parenthesise (prec > 0) (Printf.sprintf "_ => %s" b)
  | Application (f, x) ->
      let f = term 1 globals locals bound f in
      let x = atom globals locals bound x in
      parenthesise (prec > 1) (Printf.sprintf "%s %s" f x)

and atom global_name local_name bound tm =
  match tm with
  | Variable _ | Constant _ | Meta_variable _ | Sort _ | Bad ->
      term 0 global_name local_name bound tm
  | _ -> term 10 global_name local_name bound tm

and variable locals bound i =
  if i < bound then "?bad"
  else
    match locals (i - bound) with
    | Some n -> n
    | None -> "?var" ^ string_of_int (i - bound)

and parenthesise cond s = if cond then "(" ^ s ^ ")" else s

let term globals locals tm = term 0 globals locals 0 tm
