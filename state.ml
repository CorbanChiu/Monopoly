open Nativeint
open Random

type property = Board.property

type game_state = {
  property_lst : (int * property) list;
  player_lst : (int * Player.player) list;
  next : int;
}

type action = Board.action

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
let new_propertylist_update_owner
    index_value
    owner_name
    old_property
    property_list =
  let new_property =
    Board.update_property_new_owner old_property owner_name
  in

  List.remove_assoc index_value property_list
  |> List.cons (index_value, new_property)

let remove_option_owner_name owner_option =
  match owner_option with
  | None -> failwith "should not get here"
  | Some owner -> owner

(* let buy gs = let player_index = get_player_index gs.next
   gs.player_lst in let player_name = remove_option_owner_name
   (get_player_name gs.next gs.player_lst) in let old_property =
   get_property player_index gs.property_lst in let new_property_list =
   new_propertylist_update_owner player_index player_name old_property
   gs.property_lst in "need to update player list -- need to do same
   thing as you did with property list" *)
