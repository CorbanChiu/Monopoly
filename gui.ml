open Graphics

let const_board_path = "board_monopoly.json"

let board = Board.from_json (Yojson.Basic.from_file const_board_path)

let msquare_name_lst = Board.namelist board

let msquare_price_lst = Board.pricelist board

let msquare_color_lst = Board.colorlist board

type selection = Board.square option

type coord = int * int

type rect = {
  index : int option;
  lb : coord;
  lt : coord;
  rb : coord;
  rt : coord;
  orient : string;
}

let open_window () =
  open_graph " 1100x700+100-100";
  set_window_title "Monopoly"

type res_data = {
  window_size : int * int;
  buffer : int;
  valid_height : int;
  valid_width : int;
  board_height : int;
  board_width : int;
  square_w : int;
  square_h : int;
  square_diff : int;
  board_l : int;
  board_b : int;
  color_h : int;
  landscape : bool;
}

let calc_window_size () = (size_x (), size_y ())

let calc_window_buffer () = snd (calc_window_size ()) / 20

let calc_valid_height () =
  snd (calc_window_size ()) - (2 * calc_window_buffer ())

let calc_valid_width () =
  fst (calc_window_size ()) - (2 * calc_window_buffer ())

let calc_board_height () =
  min (calc_valid_width ()) (calc_valid_height ())

let calc_board_width () =
  min (calc_valid_width ()) (calc_valid_height ())

let calc_square_w () = calc_board_width () / 12

let calc_square_h () = calc_board_height () / 8

let calc_square_diff () = calc_square_h () - calc_square_w ()

let calc_board_l () =
  ((calc_valid_width () - calc_board_width ()) / 2)
  + calc_window_buffer ()

let calc_board_b () =
  ((calc_valid_height () - calc_board_height ()) / 2)
  + calc_window_buffer ()

let calc_color_h () = calc_square_h () / 5

let current_res () =
  {
    window_size = calc_window_size ();
    buffer = calc_window_buffer ();
    valid_height = calc_valid_height ();
    valid_width = calc_valid_width ();
    board_height = calc_board_height ();
    board_width = calc_board_width ();
    square_w = calc_square_w ();
    square_h = calc_square_h ();
    square_diff = calc_square_diff ();
    board_l = calc_board_l ();
    board_b = calc_board_b ();
    color_h = calc_color_h ();
    landscape = fst (calc_window_size ()) > snd (calc_window_size ());
  }

let construct_rect res (n : int) =
  match n with
  | bottomright when n = 0 ->
      {
        index = Some n;
        lb =
          (res.board_l + (9 * res.square_w) + res.square_h, res.board_b);
        lt =
          ( res.board_l + (9 * res.square_w) + res.square_h,
            res.board_b + res.square_h );
        rb =
          ( res.board_l + (9 * res.square_w) + (2 * res.square_h),
            res.board_b );
        rt =
          ( res.board_l + (9 * res.square_w) + (2 * res.square_h),
            res.board_b + res.square_h );
        orient = "corner";
      }
  | bottom when n < 10 ->
      {
        index = Some n;
        lb =
          ( res.board_l
            + ((9 - (n mod 10)) * res.square_w)
            + res.square_h,
            res.board_b );
        lt =
          ( res.board_l
            + ((9 - (n mod 10)) * res.square_w)
            + res.square_h,
            res.board_b + res.square_h );
        rb =
          ( res.board_l
            + ((9 - (n mod 10)) * res.square_w)
            + res.square_w + res.square_h,
            res.board_b );
        rt =
          ( res.board_l
            + ((9 - (n mod 10)) * res.square_w)
            + res.square_w + res.square_h,
            res.board_b + res.square_h );
        orient = "bot";
      }
  | bottomleft when n = 10 ->
      {
        index = Some n;
        lb = (res.board_l, res.board_b);
        lt = (res.board_l, res.board_b + res.square_h);
        rb = (res.board_l + res.square_h, res.board_b);
        rt = (res.board_l + res.square_h, res.board_b + res.square_h);
        orient = "corner";
      }
  | left when n < 20 ->
      {
        index = Some n;
        lb =
          ( res.board_l,
            res.board_b + (n mod 10 * res.square_w) + res.square_diff );
        lt =
          ( res.board_l,
            res.board_b
            + (n mod 10 * res.square_w)
            + res.square_w + res.square_diff );
        rb =
          ( res.board_l + res.square_h,
            res.board_b + (n mod 10 * res.square_w) + res.square_diff );
        rt =
          ( res.board_l + res.square_h,
            res.board_b
            + (n mod 10 * res.square_w)
            + res.square_w + res.square_diff );
        orient = "left";
      }
  | topleft when n = 20 ->
      {
        index = Some n;
        lb =
          (res.board_l, res.board_b + (9 * res.square_w) + res.square_h);
        lt =
          ( res.board_l,
            res.board_b + (9 * res.square_w) + (2 * res.square_h) );
        rb =
          ( res.board_l + res.square_h,
            res.board_b + (9 * res.square_w) + res.square_h );
        rt =
          ( res.board_l + res.square_h,
            res.board_b + (9 * res.square_w) + (2 * res.square_h) );
        orient = "corner";
      }
  | top when n < 30 ->
      {
        index = Some n;
        lb =
          ( res.board_l + res.square_diff + (n mod 10 * res.square_w),
            res.board_b + (9 * res.square_w) + res.square_h );
        lt =
          ( res.board_l + res.square_diff + (n mod 10 * res.square_w),
            res.board_b + (9 * res.square_w) + (2 * res.square_h) );
        rb =
          ( res.board_l + res.square_diff
            + (n mod 10 * res.square_w)
            + res.square_w,
            res.board_b + (9 * res.square_w) + res.square_h );
        rt =
          ( res.board_l + res.square_diff
            + (n mod 10 * res.square_w)
            + res.square_w,
            res.board_b + (9 * res.square_w) + (2 * res.square_h) );
        orient = "top";
      }
  | topright when n = 30 ->
      {
        index = Some n;
        lb =
          ( res.board_l + (9 * res.square_w) + res.square_h,
            res.board_b + (9 * res.square_w) + res.square_h );
        lt =
          ( res.board_l + (9 * res.square_w) + res.square_h,
            res.board_b + (9 * res.square_w) + (2 * res.square_h) );
        rb =
          ( res.board_l + (9 * res.square_w) + (2 * res.square_h),
            res.board_b + (9 * res.square_w) + res.square_h );
        rt =
          ( res.board_l + (9 * res.square_w) + (2 * res.square_h),
            res.board_b + (9 * res.square_w) + (2 * res.square_h) );
        orient = "corner";
      }
  | right when n < 40 ->
      {
        index = Some n;
        lb =
          ( res.board_l + (9 * res.square_w) + res.square_h,
            res.board_b + res.square_h
            + ((9 - (n mod 10)) * res.square_w) );
        lt =
          ( res.board_l + (9 * res.square_w) + res.square_h,
            res.board_b + res.square_h
            + ((10 - (n mod 10)) * res.square_w) );
        rb =
          ( res.board_l + (9 * res.square_w) + (2 * res.square_h),
            res.board_b + res.square_h
            + ((9 - (n mod 10)) * res.square_w) );
        rt =
          ( res.board_l + (9 * res.square_w) + (2 * res.square_h),
            res.board_b + res.square_h
            + ((10 - (n mod 10)) * res.square_w) );
        orient = "right";
      }
  | _ -> failwith "bad shape"

let get_rect_x r = fst r.lb

let get_rect_y r = snd r.lb

let get_rect_w r = fst r.rb - fst r.lb

let get_rect_h r = snd r.lt - snd r.lb

let get_x = fst

let get_y = snd

let draw_one_msquare rect =
  set_color (rgb 0 0 0);
  draw_rect (get_rect_x rect) (get_rect_y rect) (get_rect_w rect)
    (get_rect_h rect)

let draw_all_msquares msquarelst = List.iter draw_one_msquare msquarelst

let construct_msquares () =
  List.init 40 (construct_rect (current_res ()))

let color_area rect res =
  match rect with
  | bot when rect.orient = "bot" ->
      Some
        {
          index = None;
          lb = (get_x rect.lt, get_y rect.lt - res.color_h);
          lt = rect.lt;
          rb = (get_x rect.rt, get_y rect.rt - res.color_h);
          rt = rect.rt;
          orient = "color";
        }
  | left when rect.orient = "left" ->
      Some
        {
          index = None;
          lb = (get_x rect.rb - res.color_h, get_y rect.rb);
          lt = (get_x rect.rt - res.color_h, get_y rect.rt);
          rb = rect.rb;
          rt = rect.rt;
          orient = "color";
        }
  | top when rect.orient = "top" ->
      Some
        {
          index = None;
          lb = rect.lb;
          lt = (get_x rect.lb, get_y rect.lb + res.color_h);
          rb = rect.rb;
          rt = (get_x rect.rb, get_y rect.lb + res.color_h);
          orient = "color";
        }
  | right when rect.orient = "right" ->
      Some
        {
          index = None;
          lb = rect.lb;
          lt = rect.lt;
          rb = (get_x rect.lb + res.color_h, get_y rect.lb);
          rt = (get_x rect.lt + res.color_h, get_y rect.lt);
          orient = "color";
        }
  | _ -> None

let draw_one_color rect c =
  match c with
  | Some (r, g, b) -> (
      set_color (rgb r g b);
      match color_area rect (current_res ()) with
      | Some r ->
          fill_rect (get_rect_x r) (get_rect_y r) (get_rect_w r)
            (get_rect_h r)
      | None -> ())
  | None -> ()

let draw_all_colors rlst clst = List.iter2 draw_one_color rlst clst

let square_hover_aux m cd =
  match m with
  | x, y
    when x >= get_x cd.lb
         && x <= get_x cd.rt
         && y >= get_y cd.lb
         && y <= get_y cd.rt ->
      true
  | x, y -> false

let square_hover m clst =
  try Some (List.find (square_hover_aux m) clst)
  with Not_found -> None

let index_of_rect r rlst =
  let rec index_of_rect_aux r rlist n =
    if List.nth rlst n = r then Some n
    else index_of_rect_aux r rlist (n + 1)
  in
  try index_of_rect_aux r rlst 0 with _ -> None

let draw_name r rlst nmlst =
  match index_of_rect r rlst with
  | Some n ->
      (* TODO: figure out how to center this *)
      moveto 500 400;
      draw_string (List.nth nmlst n)
  | None -> ()

let draw_price r rlst plst =
  match index_of_rect r rlst with
  | Some n -> (
      match List.nth plst n with
      | Some p ->
          (* TODO: figure out how to center this *)
          moveto 500 350;
          draw_string (p |> string_of_int)
      | None -> ())
  | None -> ()

let mouseloc_handler m msqlst =
  match square_hover m msqlst with
  | Some r ->
      draw_name r msqlst msquare_name_lst;
      draw_price r msqlst msquare_price_lst;
      set_color (rgb 200 200 200);
      fill_rect (get_rect_x r) (get_rect_y r) (get_rect_w r)
        (get_rect_h r)
  | None -> ()

let draw_selection_name msquare =
  (* TODO make this location static *)
  moveto 400 400;
  draw_string (Board.get_name board msquare)

let mousepress_handler_aux m msqlst : selection =
  try
    match List.find (square_hover_aux m) msqlst with
    | { index } -> (
        match index with
        | Some n -> Some (Board.get_square board n)
        | None -> None)
  with Not_found -> None

let mousepress_handler m msqlst =
  match mousepress_handler_aux m msqlst with
  | Some msquare -> draw_selection_name msquare
  | None -> ()

let key_input char =
  match read_key () with a when a = char -> true | _ -> false

let modulo x y = if x mod y < 0 then (x mod y) + y else x mod y

(* let move_index ind dr = List.nth coords_list (modulo (ind + dr) 40) *)

let draw_token r =
  draw_circle
    (get_rect_x r + (get_rect_w r / 2))
    (get_rect_y r + (2 * get_rect_h r / 5))
    (get_rect_w r / 10)

(* TODO: Temporary implementation of draw_state *)
let rec draw_state (state : State.game_state) =
  match key_input 'p' with
  | true ->
      let msquare_lst = construct_msquares () in

      let dr = State.roll_dice () in
      let np = State.next_player state in
      let new_index player dr =
        List.nth msquare_lst (modulo (Player.position player + dr) 40)
      in
      (* let nc = (get_rect_x (new_index np dr), get_rect_y (new_index
         np dr)) in *)
      moveto (calc_board_l () - 200) (calc_board_b () + 100);
      draw_string ("Dice Roll: " ^ string_of_int dr);
      draw_token (new_index np dr);
      draw_state
        (State.move state
           [ Player.move np (modulo (Player.position np + dr) 40) ])
  | false -> ()

let unsync () =
  clear_graph ();
  let msquare_lst = construct_msquares () in
  draw_all_colors msquare_lst msquare_color_lst;
  draw_all_msquares msquare_lst;
  let st = wait_next_event [ Poll ] in
  if st.key = 'e' then raise Exit;
  if st.key = 'p' then draw_state State.init;
  if st.button then
    mousepress_handler (st.mouse_x, st.mouse_y) msquare_lst;
  mouseloc_handler (st.mouse_x, st.mouse_y) msquare_lst;
  draw_all_colors msquare_lst msquare_color_lst;
  draw_all_msquares msquare_lst

let looping () =
  try
    while true do
      unsync ();
      synchronize ()
    done
  with Exit -> ()

let draw_background =
  open_window ();
  set_line_width 2;
  let msquare_lst = construct_msquares () in
  draw_all_colors msquare_lst msquare_color_lst;
  draw_all_msquares msquare_lst;
  auto_synchronize false;
  looping ()
