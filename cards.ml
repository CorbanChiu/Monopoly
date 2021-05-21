open Yojson.Basic.Util

type card = {
  name : string;
  desc : string;
}

type cardpile = card Queue.t

let take_topcard cardpile =
  (* Note: this allows for the creation of multiple GOJF cards *)
  let top = Queue.pop cardpile in
  Queue.push top cardpile;
  top

let to_card j =
  {
    name = j |> member "name" |> to_string;
    desc = j |> member "description" |> to_string;
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
  Random.self_init ();
  let assoc_list = List.map (fun elt -> (Random.bits (), elt)) lst in
  let shuffled = List.sort compare_aux assoc_list in
  List.map snd shuffled

let from_json j =
  j |> Yojson.Basic.from_file |> to_list |> List.map to_card |> shuffle
  |> pile_of_list
