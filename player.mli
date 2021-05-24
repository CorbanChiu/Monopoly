(** Responsible for all player attributes and functions relating those
    attributes *)

(** [player] represents players of the game *)
type player

(** [get_name pl] returns the option name of the player pl*)
val get_name : player -> string option

(** [update_name pl s] returns player pl with an updated option name s *)
val update_name : player -> string option -> player

(** [get_position pl] returns position of player pl *)
val get_position : player -> int

(** [update_position pl i] returns player pl with position updated to i *)
val update_position : player -> int -> player

(** [get_cash pl] returns cash of player pl *)
val get_cash : player -> int

(** [increment_cash pl v] returns player pl with cash incremented by
    value v *)
val increment_cash : player -> int -> player

(** [decrement_cash pl v] returns player pl with cash decremented by
    value v *)
val decrement_cash : player -> int -> player

(** [get_property_lst pl] returns property index list of player pl *)
val get_property_lst : player -> int list

(** [get_networth pl] returns networth of player pl *)
val get_networth : player -> int

(** [add_property i pl] returns player pl with property at index i
    (based on the board) added to the players property list *)
val add_property : int -> player -> player

(** [remove_property pl i] returns player pl with property at index i
    (based on the board) removed from the players property list *)
val remove_property : player -> int -> player

(** [get_card_lst pl] returns card list of player pl *)
val get_card_lst : player -> Cards.card list

(** [add_card pl c] returns player pl with card c added to the player pl
    card list *)
val add_card : player -> Cards.card -> player

(** [remove_card pl c] returns player pl with card c removed from player
    pl card list *)
val remove_card : player -> Cards.card -> player

(** [get_jail_state pl] returns jail state of player pl *)
val get_jail_state : player -> int

(** [update_jail_state pl i] returns player pl with jail state changed
    to i *)
val update_jail_state : player -> int -> player

(** [get_bankrupt_state pl] true iff the players cash plus non-cash
    assets minus his debts is less than 0 *)
val bankrupt : player -> bool

(** [get_in_debt plr] returns the debt association list of plr, with
    first values corresponding to the following schema: 0: bank, 1:
    player 1, 2: player 2, 3: player 3, 4: player 4, 5: free parking,
    and second values corresponding to the amount of debt *)
val get_in_debt : player -> (int * int) list

(** [update_in_debt plr debt_lst] returns a player who has debt
    accounding to debt_lst *)
val update_in_debt : player -> (int * int) list -> player

(** [add_debt plr (target, amt)] returns a player who has owes amt more
    money to player target *)
val add_debt : player -> int -> int -> player

(** [remove_debt plr (target, amt)] returns a player who has owes amt
    less money to player target *)
val remove_debt : player -> int -> int -> player

(** [total_debt plr] returns an int representing the total amount of
    debt that plr owes *)
val total_debt : player -> int

(** [no_debt plr] returns true iff plr has no debt *)
val no_debt : player -> bool

(** [get_in_debt plr target ] returns the debt of plr to target, with
    target corresponding to the following schema: 0: bank, 1: player 1,
    2: player 2, 3: player 3, 4: player 4, 5: free parking *)
val get_debt : player -> int -> int

(** [get_player_from_name lst n] returns player in list with name n *)
val get_player_from_name :
  (int * player) list -> string option -> player

(** [init_player] returns a new player initialized with default
    attributes*)
val init_player : player

(** [get_gojf pl] returns the number of "get out of jail free cards"
    player pl posseses*)
val get_gojf : player -> int

(** [add_gojf pl] returns player pl with the number of "get out of jail
    free cards" incremented by 1*)
val add_gojf : player -> player

(** [remove_gojf pl] returns player pl with the number of "get out of
    jail free cards" decremented by 1*)
val remove_gojf : player -> player

(** [incr_net_worth a pl] returns player pl with player pl networth
    incremented by amount a*)
val incr_net_worth : int -> player -> player

(** [decr_net_worth a pl] returns player pl with player pl networth
    decremented by amount a*)
val decr_net_worth : int -> player -> player
