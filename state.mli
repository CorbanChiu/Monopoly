type property = Board.property

type game_state = {
  property_lst : (int * property) list;
  player_lst : (int * Player.player) list;
  next : int;
}

val get_property : int -> (int * property) list -> property

val update_property_lst :
  int -> property -> (int * property) list -> (int * property) list

val get_player : int -> (int * Player.player) list -> Player.player

val update_player_lst :
  int ->
  Player.player ->
  (int * Player.player) list ->
  (int * Player.player) list

val get_players_name : game_state -> (int * string option) list

val get_players_position : game_state -> (int * int) list

val get_players_cash : game_state -> (int * int) list

val get_square_owner : game_state -> int -> string option

val get_square_dev_lvl : game_state -> int -> int option

val get_square_mortgage_state : game_state -> int -> bool option

val init_game_state : game_state

val move : game_state -> Player.player list -> game_state

val next_player : game_state -> Player.player

val roll_dice : unit -> int

val next_player : game_state -> int -> int

(* val move : game_state -> int -> game_state *)

val init : game_state

val buy_property : game_state -> game_state

val pay_rent : game_state -> int -> game_state

val mortgage : game_state -> property -> game_state

val unmortgage : game_state -> property -> game_state

val develop_property : game_state -> property -> game_state

val good_output : game_state -> game_state

val assoc_list_length : 'a list -> int
