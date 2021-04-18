open Nativeint
open Random

type property = Board.property

type game_state = {
  property_lst : property list;
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

(* [init] is the initial game state *)
let init =
  {
    property_lst = Board.init_prop_lst init_board;
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
    next = next_player gs gs.next;
  }

let roll_dice () =
  self_init ();
  add (nativeint (of_int 5)) (nativeint (of_int 5)) |> to_int |> ( + ) 2
