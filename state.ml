open Nativeint
open Random

type property = Board.property

type game_state = {
  property_lst : (int * property) list;
  player_lst : (int * Player.player) list;
  next : int;
}

let num_players = 4

let init_board =
  Board.from_json (Yojson.Basic.from_file Consts.const_board_path)

let rec init_player_lst np =
  match np with
  | 0 -> []
  | a -> (a, Player.init_player) :: init_player_lst (a - 1)

(* [next_player gs nxt] returns the index of the next player who is not
   in jail. *)
let rec next_player gs nxt =
  let next_ind = (nxt + 1) mod num_players in
  if Player.jail (List.assoc next_ind gs.player_lst) then
    next_player gs (nxt + 1)
  else next_ind

let rec add_index p_lst ind =
  match p_lst with
  | [] -> []
  | h :: t -> (ind, h) :: add_index t (ind + 1)

(* [init] is the initial game state *)
let init =
  {
    property_lst = add_index (Board.init_prop_lst init_board) 0;
    player_lst = init_player_lst num_players;
    next = 0;
  }

let updated_player_lst ind p_lst np =
  List.remove_assoc ind p_lst |> List.cons np

(* [move gs] returns a new game state gs after a player has moved *)
let move gs =
  {
    property_lst = gs.property_lst;
    player_lst = gs.player_lst;
    next = gs.next;
  }

let possible_action gs ind = List.nth gs.property_lst ind

let end_turn gs =
  {
    property_lst = gs.property_lst;
    player_lst = gs.player_lst;
    next = next_player gs gs.next;
  }

let roll_dice () =
  self_init ();
  add (nativeint (of_int 5)) (nativeint (of_int 5)) |> to_int |> ( + ) 2

(* takes in the "next" value and a player_list from a gamestate and
   returns the index of the player associated with the "next" value*)
let rec get_player_index player_number player_list =
  match player_list with
  | (n, pl) :: t ->
      if n = player_number then Player.position pl
      else get_player_index player_number t
  | [] -> failwith "should not get here"

(* takes in the "next" value and a player_list from a gamestate and
   returns the name the player associated with the "next" value*)
let rec get_player_name player_number player_list =
  match player_list with
  | (n, pl) :: t ->
      if n = player_number then Player.name pl
      else get_player_name player_number t
  | [] -> failwith "should not get here"

(* gets property from property list given the index of the property *)
let rec get_property index_value property_list =
  match property_list with
  | (i, prop) :: t ->
      if index_value = i then prop else get_property index_value t
  | [] -> failwith "should not get here "

(* returns a new property list with the old property edited to have a
   new owner *)
let new_propertylist index_value new_property property_list =
  List.remove_assoc index_value property_list
  |> List.cons (index_value, new_property)

(* removes option *)
let remove_option a_option =
  match a_option with
  | None -> failwith "should not get here"
  | Some a -> a

(* extract player from list given the player_number (key value in
   player_list) *)
let rec get_player player_number player_list =
  match player_list with
  | (n, pl) :: t ->
      if n = player_number then pl else get_player player_number t
  | [] -> failwith "should not get here"

(* returns a bool based on if a player can afford a given property or
   not*)
let can_player_buy player property =
  let player_cash = Player.cash player in
  let square = Board.get_property_square property in
  let price = remove_option (Board.get_price square) in
  player_cash - price >= 0

(* returns a new player list with the player whose key value matches the
   player_number is removed and the new player is added*)
let new_playerlist player_number new_player player_list =
  List.remove_assoc player_number player_list
  |> List.cons (player_number, new_player)

(* An exception that can be raised by buy if player cannot afford
   property*)
exception Could_not_Afford

let buy gs =
  let player_index = get_player_index gs.next gs.player_lst in
  let player_name =
    remove_option (get_player_name gs.next gs.player_lst)
  in
  let old_property = get_property player_index gs.property_lst in
  let old_player = get_player gs.next gs.player_lst in

  if not (can_player_buy old_player old_property) then
    raise Could_not_Afford
  else
    (* Passed buy_check *)
    let new_property =
      Board.update_property_new_owner old_property player_name
    in
    let new_property_list =
      new_propertylist player_index new_property gs.property_lst
    in

    let new_square = Board.get_property_square new_property in
    let new_player = Player.add_property old_player new_square in
    let new_player_list =
      new_playerlist gs.next new_player gs.player_lst
    in
    {
      property_lst = new_property_list;
      player_lst = new_player_list;
      next = gs.next;
    }
