type cardpile

type card

type cardaction =
  | Move of int list * bool
  | Money of int list * bool
  | GOJF

val take_topcard : cardpile -> card

val name_topcard : cardpile -> string

val desc_topcard : cardpile -> string

val from_json : string -> cardpile

val get_action : card -> cardaction
