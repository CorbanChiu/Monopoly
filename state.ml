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

(* [init] is the initial game state *)
let init =
  {
    property_lst = Board.init_prop_lst init_board 0;
    player_lst = init_player_lst num_players;
    next = 0;
  }

(* [updated_playerlst ind np lst] returns a player list lst where the
   index ind contains the updated player np. *)
let update_player_lst player_ind new_player player_lst =
  List.remove_assoc player_ind player_lst
  |> List.cons (player_ind, new_player)

(* [roll_dice] returns a random integer between 2 and 12 (inclusive). *)
let roll_dice () =
  self_init ();
  add (nativeint (of_int 5)) (nativeint (of_int 5)) |> to_int |> ( + ) 2

(* [move gs lst] returns a new game state gs *)
let move gs dr = gs

let test property player =
  Board.same_group
    (Board.get_property_square property)
    (Player.properties player)

let possible_action gs ind = List.nth gs.property_lst ind

(* [next_player gs nxt] returns the index of the next player who is not
   in jail. *)
let rec next_player gs nxt =
  let next_ind = (nxt + 1) mod num_players in
  if Player.jail (List.assoc next_ind gs.player_lst) then
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

(* extract player from list given the player_number (key value in
   player_list) *)
let get_player player_ind player_lst = List.assoc player_ind player_lst

(* [get_player_pos ind lst] returns the position of the player who has
   index ind in the player list lst. *)
let get_player_pos player_ind player_lst =
  get_player player_ind player_lst |> Player.position

(* [get_player_name ind lst] returns the name of the player who has
   index ind in the player list lst. *)
let get_player_name player_ind player_lst =
  get_player player_ind player_lst |> Player.name

(* [get_property ind lst] returns the property at index ind in the
   property list lst *)
let get_property property_ind property_lst =
  List.assoc property_ind property_lst

(* [updated_propertylst ind np lst] returns a property list lst where
   the index ind contains the updated property np. *)
let update_property_lst property_ind new_property property_lst =
  List.remove_assoc property_ind property_lst
  |> List.cons (property_ind, new_property)

(* removes option *)
let remove_option a_option =
  match a_option with
  | None -> failwith "should not get here"
  | Some a -> a

(* returns a bool based on if a player can afford a given property or
   not*)
let can_player_buy player property =
  let player_cash = Player.cash player in
  let square = Board.get_property_square property in
  let price = remove_option (Board.get_price square) in
  player_cash - price >= 0

(* An exception that can be raised by buy if player cannot afford
   property*)
exception Could_not_Afford

let buy gs =
  let player_index = get_player_pos gs.next gs.player_lst in
  let player_name = get_player_name gs.next gs.player_lst in
  let old_property = get_property player_index gs.property_lst in
  let old_player = get_player gs.next gs.player_lst in

  if not (can_player_buy old_player old_property) then
    raise Could_not_Afford
  else
    (* Passed buy_check *)
    let new_property =
      Board.update_property_new_owner old_property player_name
    in
    let new_property_lst =
      update_property_lst player_index new_property gs.property_lst
    in

    let new_square = Board.get_property_square new_property in
    let new_player = Player.add_property old_player new_square in
    let new_player_lst =
      update_player_lst gs.next new_player gs.player_lst
    in
    {
      property_lst = new_property_lst;
      player_lst = new_player_lst;
      next = gs.next;
    }

let pay_rent gs =
  let tenant_index = get_player_index gs.next gs.player_lst in
  let tenant = get_player gs.next gs.player_lst in
  let prop = get_property tenant_index gs.property_lst in
  let owner_name = Board.get_property_owner prop in
  (* let prop_square = Board.get_property_square prop in *)
  let owner =
    Player.get_player_from_player_list_given_name gs.player_lst
      owner_name
  in
  let owner_number = Player.get_player_number gs.player_lst owner in

  let rent_price = 2 in
  (*TO DO*)
  let updated_tenant = Player.decrement_cash tenant rent_price in
  let updated_owner = Player.increment_cash owner rent_price in
  let update_tenant_ply_list =
    new_playerlist gs.next updated_tenant gs.player_lst
  in
  let update_owner_ply_list =
    new_playerlist owner_number updated_owner update_tenant_ply_list
  in

  {
    property_lst = gs.property_lst;
    player_lst = update_owner_ply_list;
    next = gs.next;
  }
