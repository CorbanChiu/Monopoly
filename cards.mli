(** Responsible for reading both community chest and chance cards from a
    json, and for manipulating these decks *)

(** The abstract type of value that represents a card deck/pile *)
type cardpile

(** The abstract type of value that represents a card (which serves as
    the backbone for community chest and chance cards) *)
type card

(** The type of value that represents the different categories of cards
    (Move, Money, and Get out of jail free card) *)
type cardaction =
  | Move of int list * bool
  | Money of int list * bool
  | GOJF

(** [take_topcard cp] returns the top card of deck/pile cp, while
    removing it from the top and adding it to the bottom *)
val take_topcard : cardpile -> card

(** [name_topcard cp] returns the name of the top card of the deck cp,
    without changing the deck *)
val name_topcard : cardpile -> string

(** [desc_topcard cp] returns the description of the top card of the
    deck cp, without changing the deck *)
val desc_topcard : cardpile -> string

(** [act_topcard cp] returns the action of the top card of the deck cp,
    without changing the deck *)
val act_topcard : cardpile -> cardaction

(** [from_json js] returns a cardpile based on the json input js *)
val from_json : string -> cardpile

(** [get_action c] returns the type of cardaction (type of card) of card
    c *)
val get_action : card -> cardaction
