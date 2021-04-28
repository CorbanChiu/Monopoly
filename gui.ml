open Graphics

let board =
  Board.from_json (Yojson.Basic.from_file Consts.const_board_path)

let msquare_name_lst = Board.namelist board

let msquare_price_lst = Board.pricelist board

let msquare_color_lst = Board.colorlist board

type selection = Board.square option

let sel_state = ref (None : selection)

type coord = int * int

type button = {
  l : int;
  b : int;
  w : int;
  h : int;
  action : string;
}

type rect = {
  index : int option;
  lb : coord;
  lt : coord;
  rb : coord;
  rt : coord;
  orient : string;
}

let center_text (x1, y1) (x2, y2) t =
  let w, h = text_size t in
  let rx = (x2 - x1 - w) / 2 in
  let ry = (y2 - y1 - h) / 2 in
  moveto (rx + x1) (ry + y1);
  draw_string t

let rightj_text (x1, y1) (x2, y2) t =
  let w, h = text_size t in
  let rx = x2 - w in
  let ry = (y2 - y1 - h) / 2 in
  moveto rx (ry + y1);
  draw_string t

let leftj_text (x1, y1) (x2, y2) t =
  let w, h = text_size t in
  let rx = x1 in
  let ry = (y2 - y1 - h) / 2 in
  moveto rx (ry + y1);
  draw_string t

let open_window () =
  open_graph Consts.const_window_dim;
  set_window_title Consts.const_window_name

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

let calc_color_h () = calc_square_h () / Consts.const_color_height

let calc_sel_w () =
  Consts.const_sel_size *. float_of_int (calc_board_width ())
  |> int_of_float

let calc_sel_h () =
  Consts.const_sel_size *. float_of_int (calc_board_height ())
  |> int_of_float

let calc_sel_l () =
  ((calc_board_width () - calc_sel_h ()) / 2) + calc_board_l ()

let calc_sel_b () =
  ((calc_board_height () - calc_sel_w ()) / 2) + calc_board_b ()

let calc_sel_headline_height () =
  Consts.const_sel_headline_height *. float_of_int (calc_sel_h ())
  |> int_of_float

let calc_sel_line_height () =
  Consts.const_sel_line_height *. float_of_int (calc_sel_h ())
  |> int_of_float

let calc_sel_buffer () =
  Consts.const_sel_buffer *. float_of_int (calc_sel_w ())
  |> int_of_float

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
      moveto 100 600;
      draw_string (List.nth nmlst n)
  | None -> ()

let draw_price r rlst plst =
  match index_of_rect r rlst with
  | Some n -> (
      match List.nth plst n with
      | Some p ->
          moveto 100 580;
          draw_string (p |> string_of_int)
      | None -> ())
  | None -> ()

let mouseloc_handler m msqlst =
  match square_hover m msqlst with
  | Some r ->
      draw_name r msqlst msquare_name_lst;
      draw_price r msqlst msquare_price_lst;
      set_color Consts.const_hover_color;
      fill_rect (get_rect_x r) (get_rect_y r) (get_rect_w r)
        (get_rect_h r)
  | None -> ()

let draw_selection_name () =
  match !sel_state with
  | Some msq ->
      set_color (rgb 0 0 0);
      center_text
        ( calc_sel_l (),
          calc_sel_b () + calc_sel_h ()
          - (2 * calc_sel_headline_height ()) )
        ( calc_sel_l () + calc_sel_w (),
          calc_sel_b () + calc_sel_h () - calc_sel_headline_height () )
        (Board.get_name_from_board board msq)
  | None -> ()

let draw_selection_desc () =
  let text_left = calc_sel_l () + calc_sel_buffer () in
  let text_right = calc_sel_l () + calc_sel_w () - calc_sel_buffer () in
  let text_top =
    calc_sel_b () + calc_sel_h () - (2 * calc_sel_headline_height ())
  in
  let line_1_t = text_top in
  let line_1_b = line_1_t - calc_sel_line_height () in
  let line_2_t = line_1_b in
  let line_2_b = line_2_t - calc_sel_line_height () in
  let line_3_t = line_2_b in
  let line_3_b = line_3_t - calc_sel_line_height () in
  let line_4_t = line_3_b in
  let line_4_b = line_4_t - calc_sel_line_height () in
  let line_5_t = line_4_b in
  let line_5_b = line_5_t - calc_sel_line_height () in
  let line_6_t = line_5_b in
  let line_6_b = line_6_t - calc_sel_line_height () in
  let line_7_t = line_6_b in
  let line_7_b = line_7_t - calc_sel_line_height () in
  let line_8_t = line_7_b in
  let line_8_b = line_8_t - calc_sel_line_height () in
  let line_9_t = line_8_b in
  let line_9_b = line_9_t - calc_sel_line_height () in
  match !sel_state with
  | Some sq -> (
      set_color (rgb 0 0 0);
      match Board.get_payments sq with
      | Some [ (k1, v1); (k2, v2) ] -> (
          center_text (text_left, line_1_b) (text_right, line_1_t)
            ("If " ^ string_of_int k1 ^ " 'Utility' is owned, rent is "
           ^ string_of_int v1 ^ " times the amount shown on dice.");
          center_text (text_left, line_2_b) (text_right, line_2_t)
            ("If " ^ string_of_int k1
           ^ " 'Utilities' are owned, rent is " ^ string_of_int v1
           ^ " times the amount shown on dice.");
          match Board.get_mortgage sq with
          | Some m ->
              center_text (text_left, line_7_b) (text_right, line_7_t)
                ("Mortgage Value $" ^ string_of_int m)
          | None -> ())
      | Some [ (k1, v1); (k2, v2); (k3, v3); (k4, v4) ] -> (
          leftj_text (text_left, line_1_b) (text_right, line_1_t) "Rent";
          rightj_text (text_left, line_1_b) (text_right, line_1_t)
            ("$ " ^ string_of_int v1);
          leftj_text (text_left, line_2_b) (text_right, line_2_t)
            ("If " ^ string_of_int k2 ^ " R.R.'s are owned");
          rightj_text (text_left, line_2_b) (text_right, line_2_t)
            ("$ " ^ string_of_int v2);
          leftj_text (text_left, line_3_b) (text_right, line_3_t)
            ("If " ^ string_of_int k3 ^ " R.R.'s are owned");
          rightj_text (text_left, line_3_b) (text_right, line_3_t)
            ("$ " ^ string_of_int v3);
          leftj_text (text_left, line_4_b) (text_right, line_4_t)
            ("If " ^ string_of_int k4 ^ " R.R.'s are owned");
          rightj_text (text_left, line_4_b) (text_right, line_4_t)
            ("$ " ^ string_of_int v4);
          match Board.get_mortgage sq with
          | Some m ->
              center_text (text_left, line_7_b) (text_right, line_7_t)
                ("Mortgage Value $" ^ string_of_int m)
          | None -> ())
      | Some
          [ (k0, v0); (k1, v1); (k2, v2); (k3, v3); (k4, v4); (k5, v5) ]
        ->
          center_text (text_left, line_1_b) (text_right, line_1_t)
            ("Rent $" ^ string_of_int v0);
          leftj_text (text_left, line_2_b) (text_right, line_2_t)
            ("With " ^ string_of_int k1 ^ " House");
          rightj_text (text_left, line_2_b) (text_right, line_2_t)
            ("$ " ^ string_of_int v1);
          leftj_text (text_left, line_3_b) (text_right, line_3_t)
            ("With " ^ string_of_int k2 ^ " Houses");
          rightj_text (text_left, line_3_b) (text_right, line_3_t)
            ("$ " ^ string_of_int v2);
          leftj_text (text_left, line_4_b) (text_right, line_4_t)
            ("With " ^ string_of_int k3 ^ " Houses");
          rightj_text (text_left, line_4_b) (text_right, line_4_t)
            ("$ " ^ string_of_int v3);
          leftj_text (text_left, line_5_b) (text_right, line_5_t)
            ("With " ^ string_of_int k4 ^ " Houses");
          rightj_text (text_left, line_5_b) (text_right, line_5_t)
            ("$ " ^ string_of_int v4);
          center_text (text_left, line_6_b) (text_right, line_6_t)
            ("With HOTEL $" ^ string_of_int v5);
          begin
            match Board.get_mortgage sq with
            | Some m ->
                center_text (text_left, line_7_b) (text_right, line_7_t)
                  ("Mortgage Value $" ^ string_of_int m)
            | None -> ()
          end;
          begin
            match Board.get_buildprice sq with
            | Some n ->
                center_text (text_left, line_8_b) (text_right, line_8_t)
                  ("Houses cost $" ^ string_of_int n ^ " each")
            | None -> ()
          end;
          begin
            match Board.get_buildprice sq with
            | Some n ->
                center_text (text_left, line_9_b) (text_right, line_9_t)
                  ("Hotels, $" ^ string_of_int n ^ " plus 4 houses")
            | None -> ()
          end;
          ()
      | None -> ()
      | _ -> failwith "improper payment structure")
  | None -> ()

let draw_selection_color () =
  begin
    match !sel_state with
    | Some msq -> (
        match
          List.nth msquare_color_lst (Board.find_square board msq)
        with
        | Some (r, g, b) -> set_color (rgb r g b)
        | None -> set_color (rgb 255 255 255))
    | None -> set_color (rgb 255 255 255)
  end;
  fill_rect (calc_sel_l ())
    (calc_sel_b () + calc_sel_h () - calc_sel_headline_height ())
    (calc_sel_w ())
    (calc_sel_headline_height ())

let is_selected sq =
  match !sel_state with Some n -> n = sq | None -> false

let selection_handler m msqlst =
  try
    match List.find (square_hover_aux m) msqlst with
    | { index } -> (
        match index with
        | Some n ->
            let sq = Board.get_square board n in
            if is_selected sq then sel_state := None
            else sel_state := (Some sq : selection)
        | None -> ())
  with Not_found -> ()

let update_sel_state st msqlst =
  if st.button then selection_handler (st.mouse_x, st.mouse_y) msqlst

(* drawn in the midle of the board, minimum value of 5 *)
let draw_selection_box () =
  try
    set_color Consts.const_sel_rect_color;
    draw_rect (calc_sel_l ()) (calc_sel_b ())
      (max (calc_sel_w ()) 2)
      (max (calc_sel_h ()) 2)
  with Invalid_argument _ -> ()

let draw_selection_bgd () =
  try
    set_color (rgb 255 255 255);
    fill_rect (calc_sel_l ()) (calc_sel_b ())
      (max (calc_sel_w ()) 2)
      (max (calc_sel_h ()) 2)
  with Invalid_argument _ -> ()

let draw_selection_fill (msqlst : rect list) =
  match !sel_state with
  | Some sq ->
      let i = Board.find_square board sq in
      let r = List.nth msqlst i in
      set_color Consts.const_sel_color;
      fill_rect (get_rect_x r) (get_rect_y r) (get_rect_w r)
        (get_rect_h r)
  | None -> ()

let btn_exit_sel () =
  {
    l = calc_sel_l () + calc_sel_w () - calc_sel_headline_height ();
    b = calc_sel_b () + calc_sel_h () - calc_sel_headline_height ();
    w = calc_sel_headline_height ();
    h = calc_sel_headline_height ();
    action = "exit selection";
  }

let draw_btn_exit_sel () =
  set_color Consts.const_exit_sel_color;
  match btn_exit_sel () with
  | { l; b; w; h } ->
      fill_rect l b w h;
      set_color (rgb 255 255 255);
      center_text (l, b) (l + w, b + h) "X"

let check_hover_button m btn =
  if
    m.mouse_x >= btn.l
    && m.mouse_x <= btn.l + btn.w
    && m.mouse_y >= btn.b
    && m.mouse_y <= btn.b + btn.h
  then sel_state := None

let button_handler st =
  if st.button then check_hover_button st (btn_exit_sel ())

let draw_selection msqlst =
  match !sel_state with
  | None -> ()
  | Some _ ->
      draw_selection_fill msqlst;
      draw_selection_bgd ();
      draw_selection_color ();
      draw_btn_exit_sel ();
      draw_selection_box ();
      draw_selection_name ();
      draw_selection_desc ()

(*********************************************************)
(**** Code that runs everthing - Add functions above ****)
(*********************************************************)

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
(* let rec draw_state (state : State.game_state) = match key_input 'p'
   with | true -> let msquare_lst = construct_msquares () in

   let dr = State.roll_dice () in let np = State.next_player state in
   let new_index player dr = List.nth msquare_lst (modulo
   (Player.position player + dr) 40) in (* let nc = (get_rect_x
   (new_index np dr), get_rect_y (new_index np dr)) in *) moveto
   (calc_board_l () - 200) (calc_board_b () + 100); draw_string ("Dice
   Roll: " ^ string_of_int dr); draw_token (new_index np dr); draw_state
   (State.move state [ Player.move np (modulo (Player.position np + dr)
   40) ]) | false -> () *)

let update () =
  clear_graph ();
  set_line_width Consts.const_line_width;
  let msquare_lst = construct_msquares () in
  let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
  (* if st.keypressed then raise Exit; *)
  button_handler st;
  update_sel_state st msquare_lst;
  mouseloc_handler (st.mouse_x, st.mouse_y) msquare_lst;
  draw_selection msquare_lst;
  draw_all_colors msquare_lst msquare_color_lst;
  draw_all_msquares msquare_lst

let driver () =
  try
    while true do
      update ();
      synchronize ()
    done
  with Exit -> ()

let init =
  open_window ();
  let msquare_lst = construct_msquares () in
  draw_all_colors msquare_lst msquare_color_lst;
  draw_all_msquares msquare_lst;
  auto_synchronize false;
  driver ()
