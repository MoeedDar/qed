let () =
  Alcotest.run "qed"
    [
      ("elaboration", Test_elaboration.tests);
      ("unification", Test_unification.tests);
      ("span", Test_span.tests);
      ("kernel", Test_kernel.tests);
    ]
