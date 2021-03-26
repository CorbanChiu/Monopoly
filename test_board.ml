open OUnit2
open Board

let board_tests = []

let suite =
  "Test suite for the Final Project" >::: List.flatten [ board_tests ]

let _ = run_test_tt_main suite
