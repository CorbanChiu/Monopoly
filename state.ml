open Nativeint
open Random

type property = Board.property

type card = {
  cc : Cards.cardpile;
  chance : Cards.cardpile;
}

type game_state = {
  property_lst : (int * property) list;
  player_lst : (int * Player.player) list;
  next : int;
  cards : card;
}

let num_houses = ref 32

let num_hotels = ref 12

let free_parking_cash = ref 0

(* [get_property ind lst] returns the property at index ind in the
   property list lst *)
let get_property property_ind property_lst =
  List.assoc property_ind property_lst

let update_property_lst property_ind new_property property_lst =
  List.remove_assoc property_ind property_lst
  |> List.cons (property_ind, new_property)

let get_player player_ind player_lst = List.assoc player_ind player_lst

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

let get_player_jail_state gs player_ind =
  Player.get_jail_state (List.assoc player_ind gs.player_lst) > 0

let flip_arg f a b = f b a

let get_players_prop gs p =
  List.map
    (flip_arg List.assoc gs.property_lst)
    (Player.get_property_lst p)

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

let name_list names = names

let init_cards =
  {
    cc = Cards.from_json Consts.const_community_chest_path;
    chance = Cards.from_json Consts.const_chance_path;
  }

let rec init_player_lst np names =
  match np with
  | 0 -> []
  | a ->
      ( a,
        Player.update_name Player.init_player
          (List.nth_opt (name_list names) (a - 1)) )
      :: init_player_lst (a - 1) names

(* [init] is the initial game state *)
let init_game_state names =
  {
    property_lst = Board.init_prop_lst init_board 0;
    player_lst = init_player_lst num_players names;
    next = 1;
    cards = init_cards;
  }

let possible_action gs ind = List.nth gs.property_lst ind

(* [next_player gs nxt] returns the index of the next player who is not
   in jail. *)
let rec next_player gs nxt = (nxt mod num_players) + 1

let end_turn gs = { gs with next = next_player gs gs.next }

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
  if Consts.demo then () else self_init ();
  ( nativeint (of_int 6) |> to_int |> ( + ) 1,
    nativeint (of_int 6) |> to_int |> ( + ) 1 )

let get_out_of_jail gs player np =
  {
    gs with
    player_lst =
      update_player_lst
        (get_player_index player gs.player_lst)
        ((Player.update_position player np |> Player.update_jail_state)
           0)
        gs.player_lst;
  }

let go_to_jail gs player =
  if Player.get_gojf player > 0 then
    let new_player = Player.remove_gojf player in
    ({
       gs with
       player_lst =
         update_player_lst
           (get_player_index player gs.player_lst)
           new_player gs.player_lst;
     }
    |> get_out_of_jail)
      new_player 10
  else
    {
      gs with
      player_lst =
        update_player_lst
          (get_player_index player gs.player_lst)
          ((Player.update_position player 10 |> Player.update_jail_state)
             3)
          gs.player_lst;
    }

let doubles_check gs player d1 d2 new_pos jail_turns =
  if d1 = d2 then get_out_of_jail gs player new_pos
  else
    {
      gs with
      player_lst =
        update_player_lst
          (get_player_index player gs.player_lst)
          (Player.update_jail_state player (jail_turns - 1))
          gs.player_lst;
    }

let pass_go gs player new_pos =
  {
    gs with
    player_lst =
      update_player_lst
        (get_player_index player gs.player_lst)
        ((Player.update_position player new_pos |> Player.increment_cash)
           200)
        gs.player_lst;
  }

let free_parking gs player =
  let fp = !free_parking_cash in
  free_parking_cash := 0;
  {
    gs with
    player_lst =
      update_player_lst
        (get_player_index player gs.player_lst)
        ((Player.update_position player 20 |> Player.increment_cash) fp)
        gs.player_lst;
  }

let move gs dr =
  let player = current_player gs in
  let d1, d2 = (fst dr, snd dr) in
  let incr = d1 + d2 + Player.get_position player in
  let new_pos = incr mod 40 in
  if new_pos = 30 then go_to_jail gs player
  else if new_pos = 20 then free_parking gs player
  else
    let jail_turns = Player.get_jail_state player in
    if jail_turns > 0 then
      doubles_check gs player d1 d2 new_pos jail_turns
    else if incr > 39 then pass_go gs player new_pos
    else
      {
        gs with
        player_lst =
          update_player_lst
            (get_player_index player gs.player_lst)
            (Player.update_position player new_pos)
            gs.player_lst;
      }

let get_property_price property =
  Board.get_sqr property |> Board.get_price |> remove_option

let buy_property gs =
  let player = current_player gs in
  let property = current_property gs in
  let mortgage_value =
    Board.get_sqr property |> Board.get_mortgage |> remove_option
  in
  let new_player =
    Player.decrement_cash player (get_property_price property)
    |> Player.add_property
         (Board.find_square init_board (Board.get_sqr property))
    |> Player.incr_net_worth mortgage_value
  in
  {
    gs with
    property_lst =
      update_property_lst
        (Player.get_position player)
        (Board.update_owner property (Player.get_name player))
        gs.property_lst;
    player_lst = update_player_lst gs.next new_player gs.player_lst;
  }

let propertylst_to_sqrlst property_lst =
  List.map Board.get_sqr property_lst

let add_rent gs dr =
  let player = current_player gs in
  let property = current_property gs in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let rent =
    Board.get_rent property
      (propertylst_to_sqrlst (get_players_prop gs owner))
      init_board dr
  in
  let new_player =
    Player.add_debt player rent (get_player_index owner gs.player_lst)
  in
  {
    gs with
    player_lst =
      update_player_lst
        (get_player_index player gs.player_lst)
        new_player gs.player_lst;
  }

let add_tax gs t =
  let player = current_player gs in
  let new_player = Player.add_debt player t 5 in
  {
    gs with
    player_lst =
      update_player_lst
        (get_player_index player gs.player_lst)
        new_player gs.player_lst;
  }

let add_luxury_tax gs = add_tax gs 75

let add_income_tax gs = add_tax gs 200

let pay_aux gs j =
  let i = gs.next in
  let i_player = current_player gs in
  let amt = Player.get_debt i_player j in
  if Player.get_cash i_player >= amt then
    let new_i_player =
      (Player.decrement_cash i_player amt |> Player.remove_debt) amt j
    in
    let new_player_lst =
      update_player_lst i new_i_player gs.player_lst
    in
    if j > 0 && j < 5 then
      let j_player = get_player j gs.player_lst in
      let new_j_player = Player.increment_cash j_player amt in
      {
        gs with
        player_lst = update_player_lst j new_j_player new_player_lst;
      }
    else if j = 5 then (
      free_parking_cash := !free_parking_cash + amt;
      { gs with player_lst = new_player_lst })
    else { gs with player_lst = new_player_lst }
  else gs

let pay gs = List.fold_left pay_aux gs [ 0; 1; 2; 3; 4; 5 ]

let mortgage gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let mortgage_value =
    Board.get_sqr property |> Board.get_mortgage |> remove_option
  in
  {
    gs with
    property_lst =
      update_property_lst property_ind
        (Board.update_mortgage_state property (Some true))
        gs.property_lst;
    player_lst =
      update_player_lst
        (get_player_index owner gs.player_lst)
        (Player.increment_cash owner mortgage_value
        |> Player.decr_net_worth mortgage_value)
        gs.player_lst;
  }

let unmortgage_output
    property
    owner
    mortgage_value
    mortgage_payment
    gs
    property_ind =
  {
    gs with
    property_lst =
      update_property_lst property_ind
        (Board.update_mortgage_state property (Some false))
        gs.property_lst;
    player_lst =
      update_player_lst
        (get_player_index owner gs.player_lst)
        (Player.decrement_cash owner mortgage_payment
        |> Player.incr_net_worth mortgage_value)
        gs.player_lst;
  }

let unmortgage gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let mortgage_value =
    Board.get_sqr property |> Board.get_mortgage |> remove_option
  in
  let mortgage_payment =
    mortgage_value |> Float.of_int |> ( *. ) 1.1 |> Float.to_int
  in
  unmortgage_output property owner mortgage_value mortgage_payment gs
    property_ind

let develop_helper_output
    owner
    new_property
    new_property_list
    house_price
    new_owner
    change
    gs =
  let new_player_list =
    update_player_lst
      (get_player_index owner gs.player_lst)
      new_owner gs.player_lst
  in
  change;
  {
    gs with
    property_lst = new_property_list;
    player_lst = new_player_list;
  }

let develop_helper gs property prop_index change =
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let old_dev_lvl = remove_option (Board.get_dev_lvl property) in
  let new_property =
    Board.update_dev_lvl property (Some (old_dev_lvl + 1))
  in
  let new_property_list =
    update_property_lst prop_index new_property gs.property_lst
  in
  let house_price =
    remove_option (Board.get_buildprice (Board.get_sqr property))
  in
  let new_owner =
    Player.decrement_cash owner house_price
    |> Player.incr_net_worth (house_price / 2)
  in
  develop_helper_output owner new_property new_property_list house_price
    new_owner change gs

let develop_property gs property_ind =
  let property = get_property property_ind gs.property_lst in
  if remove_option (Board.get_dev_lvl property) = 4 then
    develop_helper gs property property_ind
      (num_houses := !num_houses + 4;
       num_hotels := !num_hotels - 1)
  else
    develop_helper gs property property_ind
      (num_houses := !num_houses - 1)

let undevelop_helper_output
    owner
    new_property
    new_property_list
    house_price
    new_owner
    change
    gs =
  let new_player_list =
    update_player_lst
      (get_player_index owner gs.player_lst)
      new_owner gs.player_lst
  in
  change;
  {
    gs with
    property_lst = new_property_list;
    player_lst = new_player_list;
  }

let undevelop_helper gs property prop_index change =
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let old_dev_lvl = remove_option (Board.get_dev_lvl property) in
  let new_property =
    Board.update_dev_lvl property (Some (old_dev_lvl - 1))
  in
  let new_property_list =
    update_property_lst prop_index new_property gs.property_lst
  in
  let house_price =
    remove_option (Board.get_buildprice (Board.get_sqr property))
  in
  let new_owner =
    Player.increment_cash owner (house_price / 2)
    |> Player.decr_net_worth (house_price / 2)
  in
  undevelop_helper_output owner new_property new_property_list
    house_price new_owner change gs

let undevelop_property gs property_ind =
  let property = get_property property_ind gs.property_lst in
  if remove_option (Board.get_dev_lvl property) = 5 then
    undevelop_helper gs property property_ind
      (num_houses := !num_houses - 4;
       num_hotels := !num_hotels + 1)
  else
    undevelop_helper gs property property_ind
      (num_houses := !num_houses + 1)

let assoc_sort assoc_list =
  List.sort (fun (k1, v1) (k2, v2) -> Int.compare k1 k2) assoc_list

let good_output gs =
  let sorted_prop_list = assoc_sort gs.property_lst in
  let sorted_player_list = assoc_sort gs.player_lst in
  {
    gs with
    property_lst = sorted_prop_list;
    player_lst = sorted_player_list;
  }

let assoc_list_length lst =
  let rec helper lst acc =
    match lst with a :: t -> helper t (acc + 1) | [] -> acc
  in
  helper lst 0

let can_buy_property gs =
  let player = current_player gs in
  let property = current_property gs in
  if Board.get_action property (Player.get_name player) = Buy_ok then
    if Player.get_cash player - get_property_price property >= 0 then
      true
    else false
  else false

let can_pay_tax gs ok =
  let player = current_player gs in
  let property = current_property gs in
  Board.get_action property (Player.get_name player) = ok

let can_pay_luxury gs = can_pay_tax gs Luxurytax_ok

let can_pay_income gs = can_pay_tax gs Incometax_ok

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
        (propertylst_to_sqrlst (get_players_prop gs owner))
        init_board dr
    in
    Player.get_cash player - rent >= 0

let can_mortgage gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner_name = Board.get_owner property in
  let get_action_variant = Board.get_action property owner_name in
  if
    get_action_variant = Mortgage_ok
    || get_action_variant = Mortgage_and_Develop_ok
  then
    let owner =
      Player.get_player_from_name gs.player_lst
        (Board.get_owner property)
    in
    Board.check_no_development property (get_players_prop gs owner)
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

let can_develop_property_output property owner_name gs property_ind =
  let owner = Player.get_player_from_name gs.player_lst owner_name in
  if
    Player.get_cash owner - get_property_buildprice property >= 0
    && Board.complete_propertygroup property
         (propertylst_to_sqrlst (get_players_prop gs owner))
         init_board
    && Board.check_equal_development property
         (get_players_prop gs owner)
    && Board.check_no_mortgages property (get_players_prop gs owner)
    && (remove_option (Board.get_dev_lvl property) < 4
        && !num_houses > 0
       || remove_option (Board.get_dev_lvl property) = 4
          && !num_hotels > 0)
  then true
  else false

let can_develop_property gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner_name = Board.get_owner property in
  if
    Board.get_action property owner_name = Develop_and_Undevelop_ok
    || Board.get_action property owner_name = Mortgage_and_Develop_ok
  then can_develop_property_output property owner_name gs property_ind
  else false

let can_undevelop_property gs property_ind =
  let property = get_property property_ind gs.property_lst in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  if
    Board.get_action property (Player.get_name owner) = Undevelop_ok
    || Board.get_action property (Player.get_name owner)
       = Develop_and_Undevelop_ok
       && Board.check_equal_undevelopment property
            (get_players_prop gs owner)
  then true
  else false

let other_players_aux plr1 plr_elt2 =
  match plr_elt2 with i2, plr2 -> plr1 <> plr2

let add_debt_aux plr1 amt plrlst plr_elt2 =
  match plr_elt2 with i2, plr2 -> (i2, Player.add_debt plr1 amt i2)

let add_debt_all_players gs plr amt =
  let other_players =
    List.filter (other_players_aux plr) gs.player_lst
  in

  let rec aux gmst other_plrs =
    match other_plrs with
    | (i2, pl2) :: t ->
        ({
           gs with
           player_lst =
             update_player_lst gmst.next
               (Player.add_debt (current_player gmst) amt i2)
               gmst.player_lst;
         }
        |> aux)
          t
    | [] -> gmst
  in
  aux gs other_players

let jail_move_aux truth index = index = 10 || truth

let closest_move gs loc_lst =
  let curr_loc = Player.get_position (current_player gs) in
  if List.fold_left jail_move_aux false loc_lst then
    go_to_jail gs (current_player gs)
  else
    let distance =
      (List.map (fun x -> (40 + x - curr_loc) mod 40) loc_lst
      |> List.sort Stdlib.compare
      |> List.nth)
        0
    in
    move gs (distance, 0)

let process_card_output_helper lst b gs p card player_ind h =
  {
    gs with
    player_lst =
      update_player_lst player_ind
        (Player.add_debt p (-h) 5)
        gs.player_lst;
  }

let process_card_output lst b gs p card player_ind =
  match lst with
  | [ h; t ] -> gs
  | [ h ] ->
      if h < 0 then
        if b then
          process_card_output_helper lst b gs p card player_ind h
        else add_debt_all_players gs p (-h)
      else if b then
        {
          gs with
          player_lst =
            update_player_lst player_ind
              (Player.increment_cash p h)
              gs.player_lst;
        } (* increase money p by h *)
      else failwith "This isn't an allowable card anymore"
  | _ -> failwith "improperly formatted card money list"

let process_card gs p card =
  let player_ind = get_player_index p gs.player_lst in
  match Cards.get_action card with
  | Move (lst, b) -> closest_move gs lst
  | Money (lst, b) -> process_card_output lst b gs p card player_ind
  | GOJF ->
      let new_player = Player.add_gojf p in
      {
        gs with
        player_lst =
          update_player_lst player_ind new_player gs.player_lst;
      }

let process_cc gs p = process_card gs p (Cards.take_topcard gs.cards.cc)

let process_chance gs p =
  process_card gs p (Cards.take_topcard gs.cards.chance)

let cards gs =
  let player = current_player gs in
  let name_opt = Player.get_name player in
  let prop = current_property gs in
  if Board.get_action prop name_opt = CC_ok then process_cc gs player
  else if Board.get_action prop name_opt = Chance_ok then
    process_chance gs player
  else gs

let on_cc gs opt =
  match opt with
  | Some (d1, d2) ->
      let gs = move gs (d1, d2) in
      let action =
        Board.get_action (current_property gs)
          (Player.get_name (current_player gs))
      in
      action = CC_ok
  | None -> false

let on_chance gs opt =
  match opt with
  | Some (d1, d2) ->
      let gs = move gs (d1, d2) in
      let action =
        Board.get_action (current_property gs)
          (Player.get_name (current_player gs))
      in
      action = Chance_ok
  | None -> false

let get_cc_pile gs = gs.cards.cc

let get_chance_pile gs = gs.cards.chance

let free_parking gs = !free_parking_cash

let test_game_state =
  move
    (init_game_state [ "Sunny"; "Corban"; "Connor"; "Jessica" ])
    (3, 3)
  |> buy_property
  |> flip_arg move (1, 1)
  |> buy_property
  |> flip_arg move (1, 0)
  |> buy_property |> end_turn
  |> flip_arg move (6, 10)
  |> buy_property
  |> flip_arg move (1, 1)
  |> buy_property
  |> flip_arg move (1, 0)
  |> buy_property |> end_turn
  |> flip_arg move (5, 6)
  |> buy_property
  |> flip_arg move (1, 1)
  |> buy_property
  |> flip_arg move (0, 1)
  |> buy_property |> end_turn

let demo_game_state =
  move
    (init_game_state [ "Sunny"; "Corban"; "Connor"; "Jessica" ])
    (4, 2)
  |> buy_property
  |> flip_arg move (3, 2)
  |> buy_property
  |> flip_arg move (1, 1)
  |> buy_property
  |> flip_arg move (0, 1)
  |> buy_property |> end_turn
  |> flip_arg move (1, 2)
  |> buy_property
  |> flip_arg move (15, 5)
  |> buy_property
  |> flip_arg move (1, 0)
  |> buy_property
  |> flip_arg move (5, 8)
  |> buy_property
  |> flip_arg move (3, 2)
  |> end_turn
  |> flip_arg move (3, 2)
  |> buy_property
  |> flip_arg move (16, 16)
  |> end_turn
  |> flip_arg move (8, 0)
  |> buy_property
  |> flip_arg move (8, 0)
  |> buy_property
  |> flip_arg move (1, 1)
  |> buy_property
  |> flip_arg move (1, 0)
  |> buy_property
  |> flip_arg move (3, 4)
  |> buy_property
  |> flip_arg move (1, 0)
  |> buy_property
  |> flip_arg move (1, 1)
  |> buy_property
  |> flip_arg move (18, 18)
  |> buy_property |> end_turn

let game_over_aux truthy (i, plr) = truthy || Player.bankrupt plr

let game_over gs = List.fold_left game_over_aux false gs.player_lst

let cash_compare (i1, plr1) (i2, plr2) =
  let cash1 = Player.get_cash plr1 in
  let cash2 = Player.get_cash plr2 in
  Stdlib.compare cash1 cash2

let winner gs =
  match List.sort cash_compare gs.player_lst |> List.rev with
  | (i, plr) :: _ -> Player.get_name plr |> remove_option
  | _ -> ""
