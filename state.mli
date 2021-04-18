type property

type game_state

val roll_dice : unit -> int

val next_player : game_state -> int -> int

val move : game_state -> game_state

val init : game_state
