open Yojson.Basic.Util

type cardaction =
  (* This int designates the index of the square to move to and then the
     bool whether rent amount is altered *)
  | Move of int list * bool
  (* This int indicates the money gained/lost (positive/negative) and if
     its paying the bank (true) or everyone else (false) *)
  | Money of int list * bool
  (* This indicates a Get Out of Jail Free card *)
  | GOJF

type card = {
  name : string;
  desc : string;
  action : cardaction;
}

type cardpile = card Queue.t

let take_topcard cardpile =
  (* Note: this allows for the creation of multiple GOJF cards *)
  let top = Queue.pop cardpile in
  Queue.push top cardpile;
  top

let name_topcard cardpile = (Queue.peek cardpile).name

let desc_topcard cardpile = (Queue.peek cardpile).desc

let act_topcard cardpile = (Queue.peek cardpile).action

let handle_list lst = List.map (fun x -> to_int x) lst

let to_action j =
  let str = j |> member "type" |> to_string in
  match str with
  | "move" ->
      let amt = j |> member "destination" |> to_list |> handle_list in
      let altered = j |> member "altered" |> to_bool in
      Move (amt, altered)
  | "money" ->
      let amt = j |> member "amount" |> to_list |> handle_list in
      let bool = j |> member "object" |> to_bool in
      Money (amt, bool)
  | "gojf" -> GOJF
  | _ -> failwith "improper formatting of card json"

let to_card j =
  {
    name = j |> member "name" |> to_string;
    desc = j |> member "description" |> to_string;
    action = j |> member "action type" |> to_action;
  }

let pile_of_list lst =
  let p = Queue.create () in
  let rec pof_aux lst p =
    match lst with
    | h :: t ->
        Queue.push h p;
        pof_aux t p
    | [] -> p
  in
  pof_aux lst p

let compare_aux elt1 elt2 =
  match (elt1, elt2) with (k1, v1), (k2, v2) -> compare k1 k2

let shuffle lst =
  if Consts.demo then () else Random.self_init ();
  let assoc_list = List.map (fun elt -> (Random.bits (), elt)) lst in
  let shuffled = List.sort compare_aux assoc_list in
  List.map snd shuffled

let from_json j =
  j |> Yojson.Basic.from_file |> to_list |> List.map to_card |> shuffle
  |> pile_of_list

let get_action crd = crd.action
