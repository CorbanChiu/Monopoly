type player = {
  name : string option;
  token : Token.token option;
  pos : int;
  cash : int;
  property_lst : Board.property list;
  card_lst : Cards.card list;
  jail_state : bool;
  bankrupt_state : bool;
}

let get_name player = player.name

let update_name player n = { player with name = n }

let get_token player = player.token

let update_token player t = { player with token = t }

let get_position player = player.pos

let update_position player p = { player with pos = p }

let get_cash player = player.cash

let increment_cash player c = { player with cash = player.cash + c }

let decrement_cash player c = { player with cash = player.cash - c }

let get_property_lst player = player.property_lst

let add_property player p =
  { player with property_lst = p :: player.property_lst }

let remove_property player p =
  {
    player with
    property_lst = List.filter (( <> ) p) player.property_lst;
  }

let get_card_lst player = player.card_lst

let add_card player c = { player with card_lst = c :: player.card_lst }

let remove_card player c =
  { player with card_lst = List.filter (( <> ) c) player.card_lst }

let get_jail_state player = player.jail_state

let update_jail_state player b = { player with jail_state = b }

let get_bankrupt_state player = player.bankrupt_state

let update_bankrupt_state player b = { player with bankrupt_state = b }

let init_player =
  {
    name = None;
    token = None;
    pos = 0;
    cash = 0;
    property_lst = [];
    card_lst = [];
    jail_state = false;
    bankrupt_state = false;
  }

(* [move p dr] returns a new player type p after moving dr spaces. *)
let move p dr = { p with pos = dr }

(* let sum_mortgage_value property_lst = List.fold_left ( + ) 0
   property_lst *)

(* let int_of_square sq = match Board.mortgage sq with None -> 0 | Some
   x -> x *)

(* let intlist_of_squarelist lst = List.map int_of_square lst *)

(* TODO: need to include houses / hotels *)

(* let net_worth player = player.cash + sum_mortgage_value
   (intlist_of_squarelist player.property_lst) *)

(* let bankrupt player = net_worth player >= 0 *)

let rec get_player_from_name player_lst owner_option =
  match player_lst with
  | [] -> failwith "Player could not be found"
  | (_, pl) :: t ->
      if pl.name = owner_option then pl
      else get_player_from_name t owner_option

let rec get_player_number player_lst player =
  match player_lst with
  | (ind, pl) :: t ->
      if pl = player then ind else get_player_number t player
  | [] -> failwith "player could not be found"
