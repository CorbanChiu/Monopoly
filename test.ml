open OUnit2
open Board

let pp_string s = "\"" ^ s ^ "\""

let pp_int_option = function
  | Some x -> string_of_int x
  | None -> "None"

let pp_sq_loc b sq = find_square b sq |> string_of_int

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let test_board = from_json (Yojson.Basic.from_file "test_board.json")

(* ref: https://monopoly.fandom.com/wiki/Monopoly#American_board *)
let monopoly_board =
  from_json (Yojson.Basic.from_file "monopoly_board.json")

let get_square_test (name : string) (b : board) (n : int) (e : square) :
    test =
  name >:: fun _ -> assert_equal e (get_square b n)

let find_square_test (name : string) (b : board) (sq : square) (e : int)
    : test =
  name >:: fun _ -> assert_equal e (find_square b sq)

let namelist_test (name : string) (b : board) (e : string list) : test =
  name >:: fun _ ->
  assert_equal e (namelist b) ~printer:(pp_list pp_string)

let pricelist_test (name : string) (b : board) (e : int option list) :
    test =
  name >:: fun _ ->
  assert_equal e (pricelist b) ~printer:(pp_list pp_int_option)

let colorlist_test
    (name : string)
    (b : board)
    (e : propertycolor option list) : test =
  name >:: fun _ -> assert_equal e (colorlist b)

let mortgagelist_test (name : string) (b : board) (e : int option list)
    : test =
  name >:: fun _ ->
  assert_equal e (mortgagelist b) ~printer:(pp_list pp_int_option)

let propertygroup_test
    (name : string)
    (b : board)
    (s : square)
    (e : square list) : test =
  name >:: fun _ ->
  assert_equal e (propertygroup b s)
    ~printer:(pp_list (pp_sq_loc monopoly_board))

let board_tests =
  [
    (* Testing the simple board *)
    namelist_test "this tests the simple board" test_board
      [ "Boardwalk"; "Park Place" ];
    pricelist_test "this tests the simple board" test_board
      [ Some 400; Some 350 ];
    (* TODO: make this a real test *)
    colorlist_test "this tests the simple board" test_board
      (colorlist test_board);
    mortgagelist_test "this tests the simple board" test_board
      [ Some 200; Some 175 ];
    propertygroup_test "this tests the simple board" test_board
      (get_square test_board 0)
      [ get_square test_board 0; get_square test_board 1 ];
    (* Testing the real board *)
    namelist_test "Testing the names of the real monopoly board"
      monopoly_board
      [
        "Go";
        "Mediterranean Avenue";
        "Community Chest";
        "Baltic Avenue";
        "Income Tax";
        "Reading Railroad";
        "Oriental Avenue";
        "Chance";
        "Vermont Avenue";
        "Connecticut Avenue";
        "Jail";
        "St. Charles Place";
        "Electric Company";
        "States Avenue";
        "Virginia Avenue";
        "Pennsylvania Railroad";
        "St. James Place";
        "Community Chest";
        "Tennessee Avenue";
        "New York Avenue";
        "Free Parking";
        "Kentucky Avenue";
        "Chance";
        "Indiana Avenue";
        "Illinois Avenue";
        "B. & O. Railroad";
        "Atlantic Avenue";
        "Ventnor Avenue";
        "Water Works";
        "Marvin Gardens";
        "Go To Jail";
        "Pacific Avenue";
        "North Carolina Avenue";
        "Community Chest";
        "Pennsylvania Avenue";
        "Short Line";
        "Chance";
        "Park Place";
        "Luxury Tax";
        "Boardwalk";
      ];
    pricelist_test "Testing the prices of the real monopoly board"
      monopoly_board
      [
        None;
        Some 60;
        None;
        Some 60;
        None;
        Some 200;
        Some 100;
        None;
        Some 100;
        Some 120;
        None;
        Some 140;
        Some 150;
        Some 140;
        Some 160;
        Some 200;
        Some 180;
        None;
        Some 180;
        Some 200;
        None;
        Some 220;
        None;
        Some 220;
        Some 240;
        Some 200;
        Some 260;
        Some 260;
        Some 150;
        Some 280;
        None;
        Some 300;
        Some 300;
        None;
        Some 320;
        Some 200;
        None;
        Some 350;
        None;
        Some 400;
      ];
    (* TODO: figure out how to test the color function? *)
    mortgagelist_test "Testing the mortgages of the real monopoly board"
      monopoly_board
      [
        None;
        Some 30;
        None;
        Some 30;
        None;
        Some 100;
        Some 50;
        None;
        Some 50;
        Some 60;
        None;
        Some 70;
        Some 75;
        Some 70;
        Some 80;
        Some 100;
        Some 90;
        None;
        Some 90;
        Some 100;
        None;
        Some 110;
        None;
        Some 110;
        Some 120;
        Some 100;
        Some 130;
        Some 130;
        Some 75;
        Some 140;
        None;
        Some 150;
        Some 150;
        None;
        Some 160;
        Some 100;
        None;
        Some 175;
        None;
        Some 200;
      ];
    propertygroup_test
      "Testing the property groups of the yellow properties"
      monopoly_board
      (get_square monopoly_board 29)
      [
        get_square monopoly_board 26;
        get_square monopoly_board 27;
        get_square monopoly_board 29;
      ];
  ]

let suite =
  "Test suite for the Final Project" >::: List.flatten [ board_tests ]

let _ = run_test_tt_main suite
