open Js_of_ocaml

class type position = object
  method line : int Js.readonly_prop
  method character : int Js.readonly_prop
end

class type diagnostic = object
  method start : position Js.t Js.readonly_prop
  method end_ : position Js.t Js.readonly_prop
  method severity : int Js.opt Js.readonly_prop
  method message : Js.js_string Js.t Js.readonly_prop
end

class type check_result = object
  method diagnostics : diagnostic Js.t Js.js_array Js.t Js.readonly_prop
end

class type hover_result = object
  method markdown : Js.js_string Js.t Js.readonly_prop
end

class type hypothesis = object
  method name : Js.js_string Js.t Js.readonly_prop
  method typ : Js.js_string Js.t Js.readonly_prop
end

class type goal = object
  method start : position Js.t Js.readonly_prop
  method end_ : position Js.t Js.readonly_prop
  method context : hypothesis Js.t Js.js_array Js.t Js.readonly_prop
  method goal : Js.js_string Js.t Js.readonly_prop
end

class type goals_result = object
  method goals : goal Js.t Js.js_array Js.t Js.readonly_prop
end

class type api = object
  method ping : Js.js_string Js.t Js.meth
  method check : Js.js_string Js.t -> check_result Js.t Js.meth
  method hover : Js.js_string Js.t -> int -> int -> hover_result Js.t Js.meth
  method goals : Js.js_string Js.t -> int -> int -> goals_result Js.t Js.meth
end

let position (p : Lsp.position) =
  object%js
    val line = p.line
    val character = p.character
  end

let diagnostic (d : Lsp.diagnostic) =
  object%js
    val start = position d.start
    val end_ = position d.end_
    val severity = Js.some d.severity
    val message = Js.string d.message
  end

let hypothesis (h : Lsp.hypothesis) =
  object%js
    val name = Js.string h.name
    val typ = Js.string h.typ
  end

let goal (g : Lsp.goal) =
  object%js
    val start = position g.start
    val end_ = position g.end_
    val context = Js.array (Array.of_list (List.map hypothesis g.context))
    val goal = Js.string g.goal
  end

let array_of f xs = Js.array (Array.of_list (List.map f xs))

let check source =
  object%js
    val diagnostics = array_of diagnostic (Lsp.check source)
  end

let hover source line col =
  let markdown = Option.value (Lsp.hover source line col) ~default:"" in
  object%js
    val markdown = Js.string markdown
  end

let goals source _line _col =
  object%js
    val goals = array_of goal (Lsp.goals source)
  end

let api : api Js.t =
  object%js
    method ping = Js.string "hello qed"
    method check text = check (Js.to_string text)
    method hover text line col = hover (Js.to_string text) line col
    method goals text line col = goals (Js.to_string text) line col
  end

let () = Js.export "qed" api
