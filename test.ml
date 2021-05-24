open OUnit2
open Board

(************************** Our Test Process ***************************)
(* Before diving into the details of this test suite, it should be made
   clear that due to the nature of a monopoly game, much of our program
   correctness and playability can be better demonstrated and analyzed
   through playing the game itself, rather through a test suite. As
   such, much of our testing and debugging was done this way.

   Parts tested by OUnit: In OUnit testing, we focused on altering our
   gamestate (which represents a particular state of the game at a
   moment in time) using action functions (e.g. buying properties,
   developing properties) and testing if these actions were correctly
   reflected in the outputted gamestate. The first function we tested
   was "Move" which moves a player within the monopoly board based on a
   diceroll. We then tested init_game_state which outputs an initial
   gamestate representing the start of the monopoly game (where no
   player owns any properties, etc.) We then tested our main action
   functions which included buying properties, mortgaging and
   unmortgaging properties, developing and undeveloping properties and
   paying rent. We assessed the correctness of the outputted gamestate
   by manually looking through each attribute of gamestate and checking
   if changes had occured. Thus the main module tested on Ounit was
   state.ml (which alters a gamestate and moves the game forward).

   Parts Not tested by Ounit: Many components of our monopoly game were
   not tested by Ounit. In fact, many of these compoenents would be
   almost impossible to test. For example, button presses on a keyboard
   correlating to a certain action in the game, can only be tested by
   playing the game itself. Thus, many of the graphical features were
   tested by playing the game within our team and attempting to find
   corner cases. In addition, not all possible actions with all possible
   circumstantial combinations were tested either. For example,
   developing a property is a simple test (which we did test). However,
   developing a property with each different combination of property
   development levels was not tested. In addition, community chest,
   chance, "go to jail", and miscellaneous actions (like income tax and
   luxury tax) were not tested directly by Ounit. Instead they were
   tested thoroughly through gameplay. We also had some friends play our
   Monopoly game to gain an unbiased perspective on the detail,
   playability and correctness of our implementation.

   Testing Strategy: The method used for testing was primarily black box
   testing, but with the additional knowledge of the rules of the game.
   As such, many of our tests were derived so as to deal with
   potentially difficult "corner cases" that arise from the official
   rules-- like when an action is expressly prohibited/allowed. Much of
   the testing was implemented after the action functions themselves
   seemed correct and playable. Thus, our OUnit tests were conducted to
   ensure that we were properly following the rules and maintaining
   information throughout the gameplay.

   Correctness Guarantee: Our testing apporach demonstrates the
   correctness of our monopoly game. Through our two method approach of
   extensive gameplay and testing specific action function that are
   integral to the game, we were able expose corner cases, guarantee
   playability and ensure that our gamestate is correct at all moments
   in the game itself. Thus our game is fluid and able to move forward
   with any potential action. *)

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

let test_board = from_json (Yojson.Basic.from_file "board_test.json")

(* ref: https://monopoly.fandom.com/wiki/Monopoly#American_board *)
let monopoly_board =
  from_json (Yojson.Basic.from_file "board_monopoly.json")

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
        "B.&O. Railroad";
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

let test_player_cash name player_index player_list dow =
  name >:: fun _ ->
  assert_equal dow
    (Player.get_cash (State.get_player player_index player_list))

let test_property_owner name property_index property_list dow =
  name >:: fun _ ->
  assert_equal dow
    (Board.get_owner (State.get_property property_index property_list))

let test_player_properties_size name player dow =
  name >:: fun _ ->
  assert_equal dow
    (State.assoc_list_length (Player.get_property_lst player))

let test_player_index name player_index (gs : State.game_state) result =
  name >:: fun _ ->
  assert_equal result
    (Player.get_position (State.get_player player_index gs.player_lst))

let test_player_property_list_size
    name
    player_index
    (gs : State.game_state)
    result =
  name >:: fun _ ->
  assert_equal result
    (List.length
       (Player.get_property_lst
          (State.get_player player_index gs.player_lst)))

let test_player_cash name player_index (gs : State.game_state) result =
  name >:: fun _ ->
  assert_equal result
    (Player.get_cash (State.get_player player_index gs.player_lst))

let test_square_board_location
    name
    property_index
    (gs : State.game_state)
    result =
  name >:: fun _ ->
  assert_equal result
    (Board.get_name_from_square
       (Board.get_sqr (List.assoc property_index gs.property_lst)))

let test_player_property_name
    name
    player_index
    property_index
    (gs : State.game_state)
    result =
  name >:: fun _ ->
  assert_equal result
    (Board.get_name_from_square
       (Board.get_sqr
          (State.get_property
             (List.nth
                (State.get_player player_index gs.player_lst
                |> Player.get_property_lst)
                0)
             gs.property_lst)))

let test_property_owner_buy
    name
    player_index
    property_index
    (gs : State.game_state)
    result =
  name >:: fun _ ->
  assert_equal result
    (Board.get_owner
       (State.get_property
          (List.nth
             (State.get_player player_index gs.player_lst
             |> Player.get_property_lst)
             0)
          gs.property_lst))

let test_player_networth
    name
    player_index
    (gs : State.game_state)
    result =
  name >:: fun _ ->
  assert_equal result
    (Player.get_networth (State.get_player player_index gs.player_lst))

let test_player_property_mortgaged
    name
    player_index
    property_index
    (gs : State.game_state)
    result =
  name >:: fun _ ->
  assert_equal result
    (Board.get_mortgage_state
       (State.get_property
          (List.nth
             (State.get_player player_index gs.player_lst
             |> Player.get_property_lst)
             0)
          gs.property_lst))

let remove_option a =
  match a with Some r -> r | None -> failwith "what"

let test_property_dev_lvl
    name
    player_index
    property_index
    (gs : State.game_state)
    result =
  name >:: fun _ ->
  assert_equal result
    (Board.get_dev_lvl
       (State.get_property
          (List.nth
             (State.get_player player_index gs.player_lst
             |> Player.get_property_lst)
             2)
          gs.property_lst))

(************************************************************************)
(* Testing Action functions *)
(*************************************************************************)
let gs0 =
  State.init_game_state [ "Sunny"; "Connor"; "Corban"; "Jessica" ]

let gs1 = State.move gs0 (3, 2)

let gs2 = State.move (State.end_turn gs1) (5, 6)

let move_tests =
  [
    test_player_index "First move test" 1 gs1 5;
    test_player_index "second move test (next players turn)" 2 gs2 11;
  ]

let init_game_state_tests =
  [
    (* checking that player positions are all zero *)
    test_player_index "First init test" 1 gs0 0;
    test_player_index "second init test" 2 gs0 0;
    test_player_index "third init test" 3 gs0 0;
    test_player_index "fourth init test" 4 gs0 0;
    (* checking that all players' property lists are zero *)
    test_player_property_list_size "first size test" 1 gs0 0;
    test_player_property_list_size "second size test" 2 gs0 0;
    test_player_property_list_size "third size test" 3 gs0 0;
    test_player_property_list_size "fourth size test" 4 gs0 0;
    (* checking that all players' cash is zero *)
    test_player_cash "first cash test" 1 gs0 1500;
    test_player_cash "second cash test" 2 gs0 1500;
    test_player_cash "third cash test" 3 gs0 1500;
    test_player_cash "fourth cash test" 4 gs0 1500;
    (* checking if board squares have been initialized property *)
    test_square_board_location "Go" 0 gs0 "Go";
    test_square_board_location "Med ave" 1 gs0 "Mediterranean Avenue";
    test_square_board_location "Jail" 10 gs0 "Jail";
    test_square_board_location "Electric" 12 gs0 "Electric Company";
    test_square_board_location "Penn RR" 15 gs0 "Pennsylvania Railroad";
    test_square_board_location "NYA" 19 gs0 "New York Avenue";
    test_square_board_location "Free Parking" 20 gs0 "Free Parking";
    test_square_board_location "Chance" 22 gs0 "Chance";
    test_square_board_location "Water Works" 28 gs0 "Water Works";
    test_square_board_location "Go to jail" 30 gs0 "Go To Jail";
    test_square_board_location "Community Chest" 33 gs0
      "Community Chest";
    test_square_board_location "Boardwalk" 39 gs0 "Boardwalk";
  ]

let gs1 = State.move gs0 (3, 3)

let gs2 = State.buy_property gs1

let gs3 = State.move (State.end_turn gs2) (5, 6)

let gs4 = State.buy_property gs3

let buy_test =
  [
    (********** first buy test *************)
    (* cash has been decremented *)
    test_player_cash "decremented" 1 gs2 1400;
    (* player's property list size *)
    test_player_property_list_size "added 1" 1 gs2 1;
    (* player's property name *)
    test_player_property_name "first property check" 1 6 gs2
      "Oriental Avenue";
    (* property owner *)
    test_property_owner_buy "first name check" 1 6 gs2 (Some "Sunny");
    (* player net worth incremented *)
    test_player_networth "incremented networth" 1 gs2 50;
    (********** second buy test *************)
    (* cash has been decremented *)
    test_player_cash "decremented" 2 gs4 1360;
    (* player's property list size *)
    test_player_property_list_size "added 1" 2 gs4 1;
    (* player's property name *)
    test_player_property_name "first property check" 2 11 gs4
      "St. Charles Place";
    (* property owner *)
    test_property_owner_buy "first name check" 2 11 gs4 (Some "Connor");
    (* player net worth incremented *)
    test_player_networth "incremented networth" 2 gs4 70;
  ]

let gs5 = State.mortgage gs2 6

let gs6 = State.move (State.end_turn gs5) (5, 6)

let gs7 = State.mortgage (State.buy_property gs6) 11

let mortgage_test =
  [
    (********** first mortgage test *************)
    (* cash has been incremented *)
    test_player_cash "incremented" 1 gs5 1450;
    (* mortgage state *)
    test_player_property_mortgaged "true" 1 6 gs5 (Some true);
    (* player net worth decremented *)
    test_player_networth "decremented networth" 1 gs5 0;
    (********** second mortgage test *************)
    test_player_cash "incremented" 2 gs7 1430;
    (* mortgage state *)
    test_player_property_mortgaged "true" 2 11 gs7 (Some true);
    (* player net worth decremented *)
    test_player_networth "decremented networth" 2 gs7 0;
  ]

let gs8 = State.unmortgage gs5 6

let gs9 = State.unmortgage gs7 11

let unmortgage_tests =
  [
    (********** first mortgage test *************)
    (* cash has been decremented *)
    test_player_cash "incremented" 1 gs8 1395;
    (* mortgage state *)
    test_player_property_mortgaged "true" 1 6 gs8 (Some false);
    (* player net worth incremented *)
    test_player_networth "decremented networth" 1 gs8 50;
    (********** second mortgage test *************)
    (* cash has been decremented *)
    test_player_cash "incremented" 2 gs9 1353;
    (* mortgage state *)
    test_player_property_mortgaged "true" 2 6 gs9 (Some false);
    (* player net worth incremented *)
    test_player_networth "decremented networth" 2 gs9 70;
  ]

let gs1 = State.move gs0 (3, 3)

let gs2 = State.buy_property gs1

let gs3 = State.move (State.end_turn gs2) (5, 6)

let gs4 = State.buy_property gs3

let gs10 = State.move (State.end_turn gs4) (3, 3)

let gs11 = State.add_rent gs10 6 |> State.pay

let pay_rent_tests =
  [
    (********** first pay rent test *************)
    (* player cash has been decremented *)
    test_player_cash "decremented" 3 gs11 1494;
    (* owner cash has been incremented *)
    test_player_cash "incremented" 1 gs11 1406;
  ]

let gs0 = State.test_game_state

let gs1 = State.develop_property gs0 6

let gs2 = State.undevelop_property gs1 6

let develop_tests =
  [
    (********** first develop test *************)
    (* player cash has been decremented *)
    test_player_cash "decremented" 1 gs1 1130;
    (* property dev lvl has been incremented *)
    test_property_dev_lvl "dev incremented" 1 6 gs1 (Some 1);
    (* player net worth incremented *)
    test_player_networth "decremented networth" 1 gs1 185;
  ]

let undevelop_tests =
  [
    (********** first undevelop test *************)
    (* player cash has been incremented *)
    test_player_cash "decremented" 1 gs2 1155;
    (* property dev lvl has been incremented *)
    test_property_dev_lvl "dev incremented" 1 6 gs2 (Some 0);
    (* player net worth incremented *)
    test_player_networth "decremented networth" 1 gs2 160;
  ]

let suite =
  "Test suite for the Final Project"
  >::: List.flatten
         [
           board_tests;
           move_tests;
           init_game_state_tests;
           buy_test;
           mortgage_test;
           unmortgage_tests;
           pay_rent_tests;
           develop_tests;
           undevelop_tests;
         ]

let _ = run_test_tt_main suite
