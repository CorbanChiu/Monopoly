type player = {
  pos : int;
  cash : int;
  properties : Board.square list;
  cards : Cards.card list;
  jail : bool;
  token : Token.token option;
  name : string option;
  bankrupt : bool;
}

let init_player =
  {
    pos = 0;
    cash = 0;
    properties = [];
    cards = [];
    jail = false;
    token = None;
    name = None;
    bankrupt = false;
  }

(* [move p dr] returns a new player type p after moving dr spaces. *)
let move p dr = { p with pos = dr }

let position player = player.pos

let cash player = player.cash

let properties player = player.properties

let cards player = player.cards

let jail player = player.jail

let token player = player.token

let name player = player.name

let sum_mortgage_value prop = List.fold_left ( + ) 0 prop

let int_of_square sq =
  match Board.mortgage sq with None -> 0 | Some x -> x

let intlist_of_squarelist lst = List.map int_of_square lst

(* TODO: need to include houses *)
let net_worth player =
  player.cash
  + sum_mortgage_value (intlist_of_squarelist player.properties)

let bankrupt player = net_worth player >= 0

(* Helper *)
let remove_from_list lst member =
  let rec helper lst member acc =
    match lst with
    | [] -> acc
    | a :: t ->
        if a = member then helper t member acc
        else helper t member (a :: acc)
  in
  helper lst member []

let increment_cash player added_cash =
  { player with cash = player.cash + added_cash }

let decrement_cash player subtracted_cash =
  { player with cash = player.cash - subtracted_cash }

let add_card player card = { player with cards = card :: player.cards }

let remove_card player card =
  { player with cards = remove_from_list player.cards card }

let add_property player property =
  { player with properties = property :: player.properties }

let remove_property player property =
  {
    player with
    properties = remove_from_list player.properties property;
  }

let send_to_jail player = { player with jail = true }

let let_out_of_jail player = { player with jail = false }

let change_to_bankrupt player = { player with bankrupt = true }

let change_to_not_bankrupt player = { player with bankrupt = false }

let rec get_player_from_player_list_given_name player_lst owner_option =
  match player_lst with
  | [] -> failwith "player could not be found"
  | (a, pl) :: t ->
      if pl.name = owner_option then pl
      else get_player_from_player_list_given_name t owner_option

let rec get_player_number player_lst (player : player) =
  match player_lst with
  | (a, pl) :: t ->
      if pl = player then a else get_player_number t player
  | [] -> failwith "player could not be found"
