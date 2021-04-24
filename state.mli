type property

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

val roll_dice : unit -> int

val next_player : game_state -> int -> int

(* val move : game_state -> int -> game_state *)

val init : game_state

val buy_property : game_state -> game_state

val pay_rent : game_state -> int -> game_state

val mortgage : game_state -> property -> game_state

val unmortgage : game_state -> property -> game_state

val develop_property : game_state -> property -> game_state
