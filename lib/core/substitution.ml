let shift amt cutoff tm =
  let shift depth i = if i >= cutoff + depth then i + amt else i in
  Term.Binding.map shift 0 tm

let substitute f tm =
  let subst depth i =
    if i < depth then Term.Variable i else shift depth 0 (f (i - depth))
  in
  Term.Binding.bind subst 0 tm

let instantiate body var =
  let img i = if i = 0 then var else Term.Variable (i - 1) in
  substitute img body

let abstract args tm =
  let len = List.length args in
  let var i d = List.find_index (( = ) (i - d)) args in
  let shift d pos = d + len - pos - 1 in
  let abs d i =
    if i < d then i
    else match var i d with Some pos -> shift d pos | None -> i
  in
  Term.Binding.map abs 0 tm

let weaken tm = shift 1 0 tm
