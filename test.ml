open OUnit2
open Board

let test_board = from_json (Yojson.Basic.from_file "test_board.json")

let namelist_test (n : string) (b : board) (e : string list) : test =
  n >:: fun _ -> assert_equal e (namelist b)

let board_tests =
  [
    namelist_test "this tests the simple board" test_board
      [ "Boardwalk"; "Park Place" ];
  ]

let suite =
  "Test suite for the Final Project" >::: List.flatten [ board_tests ]

let _ = run_test_tt_main suite
