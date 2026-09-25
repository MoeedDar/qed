type kind' = Evaluate | Define
type kind = kind' Span.located
type 'a t' = { kind : kind; value : 'a }
type 'a t = 'a t' Span.located

let make kind value = { kind; value }

let map (cmd : 'a t) (value : 'b) : 'b t =
  { cmd with value = { cmd.value with value } }

let value (cmd : 'a t) : 'a = cmd.value.value
