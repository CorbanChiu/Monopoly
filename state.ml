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
  player_lst : Player.player;
  order : int;
}

let roll_dice () =
  add (nativeint (of_int 5)) (nativeint (of_int 5)) |> to_int |> ( + ) 2
