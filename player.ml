type player = {
  name : string option;
  pos : int;
  cash : int;
  property_lst : int list;
  card_lst : Cards.card list;
  jail_state : int;
  gojf : int;
  net_worth_diff_cash : int;
  in_debt : (int * int) list;
}

let get_name player = player.name

let update_name player n = { player with name = n }

let get_position player = player.pos

let update_position player p = { player with pos = p }

let get_cash player = player.cash

let increment_cash player c = { player with cash = player.cash + c }

let decrement_cash player c = { player with cash = player.cash - c }

let get_property_lst player = player.property_lst

let get_networth player = player.net_worth_diff_cash

let add_property p player =
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

let get_in_debt player = player.in_debt

let update_in_debt player lst = { player with in_debt = lst }

let init_player =
  {
    name = None;
    pos = 0;
    cash = 1500;
    property_lst = [];
    card_lst = [];
    jail_state = 0;
    gojf = 0;
    net_worth_diff_cash = 0;
    (* 0: bank, 1: player 1, 2: player 2, 3: player 3, 4: player 4, 5:
       free parking *)
    in_debt = [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0) ];
  }

let total_debt_aux acc elt = match elt with x, y -> y + acc

let total_debt player = List.fold_left total_debt_aux 0 player.in_debt

let bankrupt plr =
  plr.cash + plr.net_worth_diff_cash - total_debt plr < 0

let no_debt player = total_debt player = 0

let add_debt_aux amt recipient elt =
  match elt with
  | k, v when k = recipient -> (k, v + amt)
  | k, v -> (k, v)

let add_debt player amt recipient =
  {
    player with
    in_debt = List.map (add_debt_aux amt recipient) player.in_debt;
  }

let remove_debt player amt recipient =
  {
    player with
    in_debt = List.map (add_debt_aux (-amt) recipient) player.in_debt;
  }

let get_debt player recipient =
  match
    List.find
      (fun x -> match x with k, v -> k = recipient)
      player.in_debt
  with
  | k, v -> v

let rec get_player_from_name player_lst owner_option =
  match player_lst with
  | [] -> failwith "player could not be found"
  | (_, pl) :: t ->
      if pl.name = owner_option then pl
      else get_player_from_name t owner_option

let get_gojf p = p.gojf

let add_gojf p =
  let prev = p.gojf in
  { p with gojf = prev + 1 }

let remove_gojf p =
  let prev = p.gojf in
  { p with gojf = prev - 1 }

let incr_net_worth amt p =
  let prev = p.net_worth_diff_cash in
  { p with net_worth_diff_cash = prev + amt }

let decr_net_worth amt p =
  let prev = p.net_worth_diff_cash in
  { p with net_worth_diff_cash = prev - amt }
