open Nativeint
open Random

type property = {
  square : Board.square;
  (* None indicates unbuyability while Bank indicates not owned by any
     player yet *)
  owner : string option;
  dev_lvl : int option;
  mortgaged : bool;
}

type game_state = {
  property_lst : property list;
  player_lst : Player.player list;
  next : int;
}

let init =
  { property_lst = []; player_lst = [ Player.default ]; next = 0 }

let move game_state player_lst =
  {
    property_lst = game_state.property_lst;
    player_lst;
    next = game_state.next;
  }

let next_player game_state =
  List.nth game_state.player_lst game_state.next

let roll_dice () =
  self_init ();
  add (nativeint (of_int 5)) (nativeint (of_int 5)) |> to_int |> ( + ) 2
