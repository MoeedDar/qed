let position source offset =
  let document = Span.document source in
  let position = Span.position_of_offset document offset in
  (position.Span.line, position.character)

let offset source line character =
  Span.offset_of (Span.document source) line character

let check_position name source offset expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (pair int int))
        "line/character" expected (position source offset))

let check_offset name source line character expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check int) "offset" expected (offset source line character))

let tests =
  [
    check_position "start of document" "ab\ncd" 0 (0, 0);
    check_position "after a newline" "ab\ncd" 3 (1, 0);
    check_position "inside the second line" "ab\ncd" 4 (1, 1);
    check_position "end of document" "ab\ncd" 5 (1, 2);
    check_position "an offset past the end is clamped" "ab\ncd" 99 (1, 2);
    check_position "a trailing newline is a line of its own" "ab\n" 3 (1, 0);
    check_position "an empty document" "" 0 (0, 0);
    check_offset "the start of a line" "ab\ncd" 1 0 3;
    check_offset "a column within a line" "ab\ncd" 1 1 4;
    check_offset "a line past the end resolves to the end" "ab\ncd" 9 0 5;
    check_offset "the line after a trailing newline has no column" "ab\n" 1 4 3;
  ]
