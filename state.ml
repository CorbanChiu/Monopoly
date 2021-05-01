open Nativeint
open Random

type property = Board.property

type game_state = {
  property_lst : (int * property) list;
  player_lst : (int * Player.player) list;
  next : int;
}

(* [get_property ind lst] returns the property at index ind in the
   property list lst *)
let get_property property_ind property_lst =
  List.assoc property_ind property_lst

(* [updated_propertylst ind np lst] returns a property list lst where
   the index ind contains the updated property np. *)
let update_property_lst property_ind new_property property_lst =
  List.remove_assoc property_ind property_lst
  |> List.cons (property_ind, new_property)

let rec update_property_lst_given
    property_list
    old_property
    new_property =
  match property_list with
  | (a, prop) :: t ->
      if prop = old_property then
        (a, new_property) :: List.remove_assoc a property_list
      else update_property_lst_given t old_property new_property
  | [] -> failwith "cannot update property list"

let get_player player_ind player_lst = List.assoc player_ind player_lst

(* [updated_playerlst ind np lst] returns a player list lst where the
   index ind contains the updated player np. *)
let update_player_lst player_ind new_player player_lst =
  List.remove_assoc player_ind player_lst
  |> List.cons (player_ind, new_player)

let rec get_player_index player player_lst =
  match player_lst with
  | (ind, pl) :: t ->
      if pl = player then ind else get_player_index player t
  | [] -> failwith "player could not be found"

type action = Board.action

let get_players_from gs f =
  List.map (fun (ind, player) -> (ind, f player)) gs.player_lst

let get_players_name gs = get_players_from gs Player.get_name

let get_players_position gs = get_players_from gs Player.get_position

let get_players_cash gs = get_players_from gs Player.get_cash

let get_property_info_from gs ind f = f (List.assoc ind gs.property_lst)

let get_square_owner gs ind =
  get_property_info_from gs ind Board.get_owner

let get_square_dev_lvl gs ind =
  get_property_info_from gs ind Board.get_dev_lvl

let get_square_mortgage_state gs ind =
  get_property_info_from gs ind Board.get_mortgage_state

let num_players = 4

(* [init_board] is the board converted from the json file *)
let init_board =
  Board.from_json (Yojson.Basic.from_file Consts.const_board_path)

let name_list = [ "Sunny"; "Corban"; "Connor"; "Jessica" ]

let rec init_player_lst np =
  match np with
  | 0 -> []
  | a ->
      ( a,
        Player.update_name Player.init_player
          (List.nth_opt name_list (a - 1)) )
      :: init_player_lst (a - 1)

(* [init] is the initial game state *)
let init_game_state =
  {
    property_lst = Board.init_prop_lst init_board 0;
    player_lst = init_player_lst num_players;
    next = 1;
  }

let possible_action gs ind = List.nth gs.property_lst ind

(* [next_player gs nxt] returns the index of the next player who is not
   in jail. *)
let rec next_player gs nxt =
  let next_ind = (nxt mod num_players) + 1 in
  if Player.get_jail_state (List.assoc next_ind gs.player_lst) then
    next_player gs (nxt + 1)
  else next_ind

let end_turn gs =
  {
    property_lst = gs.property_lst;
    player_lst = gs.player_lst;
    next = next_player gs gs.next;
  }

(* removes option *)
let remove_option = Board.remove_option

(* An exception that can be raised by buy if player cannot afford
   property*)
exception CouldntAfford

let current_player gs = get_player gs.next gs.player_lst

let current_property gs =
  get_property (Player.get_position (current_player gs)) gs.property_lst

let current_turn_name gs = current_player gs |> Player.get_name

(* [roll_dice] returns a random integer between 2 and 12 (inclusive). *)
let roll_dice () =
  self_init ();
  ( nativeint (of_int 6) |> to_int |> ( + ) 1,
    nativeint (of_int 6) |> to_int |> ( + ) 1 )

(* [move gs dr] returns a new game state gs *)
let move gs dr =
  let player = current_player gs in
  {
    gs with
    player_lst =
      update_player_lst
        (get_player_index player gs.player_lst)
        (Player.update_position player
           ((fst dr + snd dr + Player.get_position player) mod 40))
        gs.player_lst;
  }

let get_property_price property =
  Board.get_sqr property |> Board.get_price |> remove_option

(** [buy_property gs] returns ... *)
let buy_property gs =
  let player = current_player gs in
  let property = current_property gs in
  let new_player =
    Player.decrement_cash player (get_property_price property)
    |> Player.add_property property
  in
  {
    property_lst =
      update_property_lst
        (Player.get_position player)
        (Board.update_owner property (Player.get_name player))
        gs.property_lst;
    player_lst = update_player_lst gs.next new_player gs.player_lst;
    next = gs.next;
  }

let propertylst_to_sqrlst property_lst =
  List.map Board.get_sqr property_lst

let pay_rent gs dr =
  let player = current_player gs in
  let property = current_property gs in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let rent =
    Board.get_rent property
      (propertylst_to_sqrlst (Player.get_property_lst owner))
      init_board dr
  in
  {
    property_lst = gs.property_lst;
    player_lst =
      update_player_lst gs.next
        (Player.decrement_cash player rent)
        gs.player_lst
      |> update_player_lst
           (get_player_index owner gs.player_lst)
           (Player.increment_cash owner rent);
    next = gs.next;
  }

(* TODO: for a traditional property, check that any other properties in
   the color group are not developed *)
let mortgage gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let mortgage_value =
    Board.get_sqr property |> Board.get_mortgage |> remove_option
  in
  {
    property_lst =
      update_property_lst property_ind
        (Board.update_mortgage_state property (Some true))
        gs.property_lst;
    player_lst =
      update_player_lst
        (get_player_index owner gs.player_lst)
        (Player.increment_cash owner mortgage_value)
        gs.player_lst;
    next = gs.next;
  }

let unmortgage gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let mortgage_value =
    Board.get_sqr property |> Board.get_mortgage |> remove_option
    |> Float.of_int |> ( *. ) 1.1 |> Float.to_int
  in
  {
    property_lst =
      update_property_lst property_ind
        (Board.update_mortgage_state property (Some false))
        gs.property_lst;
    player_lst =
      update_player_lst
        (get_player_index owner gs.player_lst)
        (Player.decrement_cash owner mortgage_value)
        gs.player_lst;
    next = gs.next;
  }

let num_houses = ref 32

let num_hotels = ref 12

let develop_helper gs property change =
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let old_dev_lvl = remove_option (Board.get_dev_lvl property) in
  let new_property =
    Board.update_dev_lvl property (Some (old_dev_lvl + 1))
  in
  let new_property_list =
    update_property_lst_given gs.property_lst property new_property
  in
  let new_owner =
    Player.decrement_cash owner
      (remove_option (Board.get_buildprice (Board.get_sqr property)))
  in
  let new_player_list =
    update_player_lst
      (get_player_index new_owner gs.player_lst)
      new_owner gs.player_lst
  in
  change;
  {
    property_lst = new_property_list;
    player_lst = new_player_list;
    next = gs.next;
  }

let develop_property gs property_ind =
  let property = get_property property_ind gs.property_lst in
  if remove_option (Board.get_dev_lvl property) = 4 then
    develop_helper gs property
      (num_houses := !num_houses + 4;
       num_hotels := !num_hotels - 1)
  else develop_helper gs property (num_houses := !num_houses + 1)

let undevelop_helper gs property change =
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let old_dev_lvl = remove_option (Board.get_dev_lvl property) in
  let new_property =
    Board.update_dev_lvl property (Some (old_dev_lvl + 1))
  in
  let new_property_list =
    update_property_lst_given gs.property_lst property new_property
  in
  let new_owner =
    Player.increment_cash owner
      (remove_option (Board.get_buildprice (Board.get_sqr property)) / 2)
  in
  let new_player_list =
    update_player_lst
      (get_player_index new_owner gs.player_lst)
      new_owner gs.player_lst
  in
  change;
  {
    property_lst = new_property_list;
    player_lst = new_player_list;
    next = gs.next;
  }

let undevelop_property gs property_ind =
  let property = get_property property_ind gs.property_lst in
  if remove_option (Board.get_dev_lvl property) = 5 then
    undevelop_helper gs property
      (num_houses := !num_houses - 4;
       num_hotels := !num_hotels + 1)
  else undevelop_helper gs property (num_houses := !num_houses + 1)

let assoc_sort assoc_list =
  List.sort (fun (k1, v1) (k2, v2) -> Int.compare k1 k2) assoc_list

let good_output gs =
  let sorted_prop_list = assoc_sort gs.property_lst in
  let sorted_player_list = assoc_sort gs.player_lst in
  {
    property_lst = sorted_prop_list;
    player_lst = sorted_player_list;
    next = gs.next;
  }

let assoc_list_length lst =
  let rec helper lst acc =
    match lst with a :: t -> helper t (acc + 1) | [] -> acc
  in
  helper lst 0

(* returns whether a player can afford to buy a property. TODO: change
   to net worth instead of get_cash *)
let can_buy_property gs =
  let player = current_player gs in
  let property = current_property gs in
  if Board.get_action property (Player.get_name player) = Buy_ok then
    if Player.get_cash player - get_property_price property >= 0 then
      true
    else false
  else false

(* TODO: if we fail, we need to call "Mortgage or bankrupt". This will
   be done when we add net worth *)
let can_pay_rent gs dr =
  let player = current_player gs in
  let property = current_property gs in
  if Board.get_action property (Player.get_name player) != Payrent_ok
  then false
  else
    let owner =
      Player.get_player_from_name gs.player_lst
        (Board.get_owner property)
    in
    let rent =
      Board.get_rent property
        (propertylst_to_sqrlst (Player.get_property_lst owner))
        init_board dr
    in
    Player.get_cash player - rent >= 0

let can_mortgage gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner = Board.get_owner property in
  let get_action_variant = Board.get_action property owner in
  (* print_string "before action check"; *)
  if
    get_action_variant = Mortgage_ok
    || get_action_variant = Mortgage_and_Develop_ok
  then
    let player_owner =
      Player.get_player_from_name gs.player_lst
        (Board.get_owner property)
    in
    (* print_string "after action check"; *)
    Board.check_no_development property
      (Player.get_property_lst player_owner)
  else false

let can_unmortgage gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  if Board.get_action property (Player.get_name owner) != Unmortgage_ok
  then false
  else
    let mortgage_value =
      Board.get_sqr property |> Board.get_mortgage |> remove_option
      |> Float.of_int |> ( *. ) 1.1 |> Float.to_int
    in
    Player.get_cash owner - mortgage_value >= 0

let get_property_buildprice property =
  Board.get_sqr property |> Board.get_buildprice |> remove_option

let can_develop_property gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  if
    (Board.get_action property (Player.get_name owner) = Develop_ok
    || Board.get_action property (Player.get_name owner)
       = Mortgage_and_Develop_ok)
    && Player.get_cash owner - get_property_buildprice property >= 0
    && Board.complete_propertygroup property
         (propertylst_to_sqrlst (Player.get_property_lst owner))
         init_board
    && Board.check_equal_development property
         (Player.get_property_lst owner)
    && Board.check_no_mortgages property (Player.get_property_lst owner)
  then
    if
      (remove_option (Board.get_dev_lvl property) < 4 && !num_houses > 0)
      || remove_option (Board.get_dev_lvl property) = 4
         && !num_hotels > 0
    then true
    else false
  else false

let can_undevelop_property gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  if
    Board.get_action property (Player.get_name owner) = Undevelop_ok
    && Board.check_equal_undevelopment property
         (Player.get_property_lst owner)
  then true
  else false

let switch f y x = f x y

let demo_game_state =
  move init_game_state (2, 3)
  |> buy_property
  |> switch move (5, 6)
  |> buy_property
  |> switch move (3, 2)
  |> end_turn
  |> switch move (1, 2)
  |> buy_property
  |> switch move (4, 3)
  |> switch move (2, 1)
  |> buy_property |> end_turn
  |> switch move (5, 1)
  |> buy_property
  |> switch move (4, 4)
  |> buy_property
  |> switch move (6, 4)
  |> buy_property |> end_turn
  |> switch move (5, 2)
  |> switch move (6, 6)
  |> buy_property
  |> switch move (6, 6)
  |> buy_property |> end_turn
