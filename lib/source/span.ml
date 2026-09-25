type t = { start : int; stop : int }
type 'a located = { value : 'a; span : t }

(* A source document, with the line index needed to translate between byte
   offsets and the line/character positions editors speak in. Spans stay
   offset-based; the document is what gives them a human-facing meaning. *)
type position = { line : int; character : int }
type document = { text : string; line_starts : int array }

let rec newline_offsets i length text acc =
  if i >= length then acc
  else if text.[i] = '\n' then newline_offsets (i + 1) length text (i + 1 :: acc)
  else newline_offsets (i + 1) length text acc

let document text =
  let length = String.length text in
  let starts = newline_offsets 0 length text [] in
  { text; line_starts = Array.of_list (0 :: List.rev starts) }

(* Greatest line whose start is at or before [offset]. *)
let rec line_of_offset starts lo hi offset =
  if lo > hi then lo - 1
  else
    let mid = lo + ((hi - lo) / 2) in
    if starts.(mid) <= offset then line_of_offset starts (mid + 1) hi offset
    else line_of_offset starts lo (mid - 1) offset

let position_of_offset document offset =
  let limit = String.length document.text in
  let offset = min offset limit in
  let line = line_of_offset document.line_starts 0 (Array.length document.line_starts - 1) offset in
  let character = offset - document.line_starts.(line) in
  { line; character }

(* A line past the end of the document, or the empty line after a trailing
   newline, has no column to speak of and resolves to the end of the text. *)
let offset_of document line character =
  if line < 0 || line >= Array.length document.line_starts then
    String.length document.text
  else
    let start = document.line_starts.(line) in
    if start = String.length document.text then start else start + character

let zero = { start = 0; stop = 0 }
let make start stop = { start; stop }
let locate value span = { value; span }
let merge a b = { start = a.start; stop = b.stop }
let slice source span = String.sub source span.start (span.stop - span.start)
let between a b value = locate value (merge a.span b.span)
let unlocate located = located.value

(* Match each pending value back to its located original, in source order. *)
let rec pick located pending =
  match (located, pending) with
  | [], _ -> []
  | l :: ls, p :: ps when unlocate l = p -> l :: pick ls ps
  | _ :: ls, _ -> pick ls pending
