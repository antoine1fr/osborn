open OUnit2

let suite = "Osborn tests" >::: [
  FrontMatterTests.suite;
]

let () = run_test_tt_main suite
