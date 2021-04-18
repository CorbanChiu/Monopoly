type property

type game_state

val roll_dice : unit -> int

val next_player : game_state -> Player.player

val move : game_state -> Player.player list -> game_state

val init : game_state
