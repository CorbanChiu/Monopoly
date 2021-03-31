open Yojson.Basic.Util

type card = {
  name : string;
  desc : string;
}

type cardpile = card Pile.pile

type cardtype

type cardstate = {
  cname : string;
  cdesc : string;
  cpile : cardpile;
}

let topcard crdst =
  crdst.cpile |> Pile.peek |> function
  | Some card -> card
  | None -> failwith "no card found"

let replace_topcard crdst =
  let t = topcard crdst in
  let smaller =
    match Pile.dequeue crdst.cpile with
    | Some card -> card
    | None -> failwith "no card found"
  in
  Pile.enqueue t smaller

(* this may not actually work due to get out of jail cards that the
   player keeps *)
let take_topcard crdst =
  {
    cname = (topcard crdst).name;
    cdesc = (topcard crdst).desc;
    cpile = replace_topcard crdst;
  }

let to_card j =
  {
    name = j |> member "name" |> to_string;
    desc = j |> member "description" |> to_string;
  }

let pile_of_list lst =
  let rec pof_helper lst p =
    match lst with
    | [ h ] -> Pile.enqueue h p
    | h :: t -> pof_helper t p
    | _ -> failwith "unintended"
  in
  match lst with [] -> Pile.empty | lst -> pof_helper lst Pile.empty

let from_json j =
  {
    cpile = j |> to_list |> List.map to_card |> pile_of_list;
    cname = "";
    cdesc = "";
  }
