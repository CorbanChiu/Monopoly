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

let num_players = 4

(* [init_board] is the board converted from the json file *)
let init_board =
  Board.from_json (Yojson.Basic.from_file Consts.const_board_path)

let rec init_player_lst np =
  match np with
  | 0 -> []
  | a -> (a, Player.init_player) :: init_player_lst (a - 1)

(* [init] is the initial game state *)
let init =
  {
    property_lst = Board.init_prop_lst init_board 0;
    player_lst = init_player_lst num_players;
    next = 0;
  }

(* [roll_dice] returns a random integer between 2 and 12 (inclusive). *)
let roll_dice () =
  self_init ();
  add (nativeint (of_int 5)) (nativeint (of_int 5)) |> to_int |> ( + ) 2

(* [move gs lst] returns a new game state gs *)
let move gs dr = gs

let possible_action gs ind = List.nth gs.property_lst ind

(* [next_player gs nxt] returns the index of the next player who is not
   in jail. *)
let rec next_player gs nxt =
  let next_ind = (nxt + 1) mod num_players in
  if Player.get_jail_state (List.assoc next_ind gs.player_lst) then
    next_player gs (nxt + 1)
  else next_ind

let end_turn gs =
  {
    property_lst = gs.property_lst;
    player_lst = gs.player_lst;
    next = next_player gs gs.next;
  }

let player_turn gs =
  (* let player = List.assoc gs.next gs.player_lst in let dr = roll_dice
     () in *)
  move gs gs.player_lst |> end_turn

(* removes option *)
let remove_option = Board.remove_option

let get_property_price property =
  Board.get_sqr property |> Board.get_price |> remove_option

(* returns whether a player can afford to buy a property *)
let can_buy_property player property =
  Player.get_cash player - get_property_price property >= 0

let can_pay_rent player rent = Player.get_cash player - rent >= 0

(* An exception that can be raised by buy if player cannot afford
   property*)
exception CouldntAfford

let current_player gs = get_player gs.next gs.player_lst

let current_property gs =
  get_property (Player.get_position (current_player gs)) gs.property_lst

let buy_property gs =
  let player = current_player gs in
  let property = current_property gs in
  if can_buy_property player property then
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
  else
    failwith
      "TODO: auction -> auction can also happen if player chooses not \
       to buy"

let pay_rent gs dr =
  let player = current_player gs in
  let property = current_property gs in
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let rent =
    Board.get_rent property
      (Board.propertylst_to_sqrlst (Player.get_property_lst owner))
      init_board dr
  in
  if can_pay_rent player rent then
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
  else failwith "Mortgage or bankrupt"

let mortgage gs property =
  let player = current_player gs in
  let mortgage_value =
    Board.get_sqr property |> Board.get_mortgage |> remove_option
  in
  {
    property_lst =
      update_property_lst
        (Player.get_position player)
        (Board.update_mortgage_state property (Some true))
        gs.property_lst;
    player_lst =
      update_player_lst gs.next
        (Player.increment_cash player mortgage_value)
        gs.player_lst;
    next = gs.next;
  }

let unmortgage gs property =
  let owner =
    Player.get_player_from_name gs.player_lst (Board.get_owner property)
  in
  let mortgage_value =
    Board.get_sqr property |> Board.get_mortgage |> remove_option
    |> Float.of_int |> ( *. ) 1.1 |> Float.to_int
  in
  {
    property_lst =
      update_property_lst
        (Player.get_position owner)
        (Board.update_mortgage_state property (Some false))
        gs.property_lst;
    player_lst =
      update_player_lst gs.next
        (Player.decrement_cash owner mortgage_value)
        gs.player_lst;
    next = gs.next;
  }
