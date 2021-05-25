open Graphics

let board =
  Board.from_json (Yojson.Basic.from_file Consts.const_board_path)

let msquare_name_lst = Board.namelist board

let msquare_price_lst = Board.pricelist board

let msquare_color_lst = Board.colorlist board

type selection = Board.square option

let sel_state = ref (None : selection)

let game_state = ref (State.init_game_state [])

type turn_state = {
  has_moved : bool;
  dice : (int * int) option;
  has_rolled : bool;
  num_rolls : int;
  has_picked_card : bool;
}

let game_over = ref false

let turn_state =
  ref
    ({
       has_moved = false;
       dice = None;
       has_rolled = false;
       num_rolls = 0;
       has_picked_card = false;
     }
      : turn_state)

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

let calc_middle_height () = calc_valid_height () / 2

let calc_middle_width () = calc_valid_width () / 2

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

let calc_player_name_spacing () =
  Consts.const_player_name_spacing *. float_of_int (calc_sel_h ())
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

(* This function cannot realistically be much shorter -- we need a
   pattern match for 8 conditions (4 corners and 4 sides), as well as
   the corresponding coordinate information/calculations *)
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

let draw_all_msquares msquarelst =
  List.iter draw_one_msquare msquarelst;
  moveto
    (calc_board_l () + 5)
    (calc_board_b () + calc_board_height () + -5);
  draw_string
    ("Free Parking: $" ^ string_of_int (State.free_parking !game_state))

let construct_msquares () =
  List.init 40 (construct_rect (current_res ()))

(* This function cannot easily be much shorter either, we need to match
   against the 4 sides and provide the color and coordinates of the
   little square rectangle. We have already used a helper function to
   abstract code here. *)
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

let draw_hlight_name r rlst nmlst =
  match index_of_rect r rlst with
  | Some n ->
      set_color (rgb 0 0 0);
      center_text
        ( calc_sel_l (),
          calc_sel_b () + calc_sel_h ()
          - (2 * calc_sel_headline_height ()) )
        ( calc_sel_l () + calc_sel_w (),
          calc_sel_b () + calc_sel_h () - calc_sel_headline_height () )
        (List.nth nmlst n)
  | None -> ()

let draw_hlight_price r rlst plst =
  match index_of_rect r rlst with
  | Some n -> (
      match List.nth plst n with
      | Some p ->
          set_color (rgb 0 0 0);
          center_text
            ( calc_sel_l (),
              calc_sel_b () + calc_sel_h ()
              - (2 * calc_sel_headline_height ()) )
            ( calc_sel_l () + calc_sel_w (),
              calc_sel_b () + calc_sel_h ()
              - (2 * calc_sel_headline_height ())
              - calc_sel_line_height () )
            ("Purchase Price: $" ^ (p |> string_of_int))
      | None -> ())
  | None -> ()

let mouseloc_handler m msqlst =
  match square_hover m msqlst with
  | Some r ->
      draw_hlight_name r msqlst msquare_name_lst;
      draw_hlight_price r msqlst msquare_price_lst;
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

(* This function can't really be much shorter - we've factored out a lot
   of the calculated locations, but it requires the drawing and pattern
   matching of lots of lines of writing. *)
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
  let line_10_t = line_9_b in
  let line_10_b = line_10_t - calc_sel_line_height () in
  match !sel_state with
  | Some sq -> (
      set_color (rgb 0 0 0);
      match Board.get_payments sq with
      | Some [ (k1, v1); (k2, v2) ] -> (
          center_text (text_left, line_1_b) (text_right, line_1_t)
            ("If " ^ string_of_int k1 ^ " 'Utility' is owned");
          center_text (text_left, line_2_b) (text_right, line_2_t)
            ("rent is " ^ string_of_int v1 ^ " times");
          center_text (text_left, line_3_b) (text_right, line_3_t)
            "shown on dice.";
          center_text (text_left, line_5_b) (text_right, line_5_t)
            ("If " ^ string_of_int k2 ^ " 'Utilities' are owned");
          center_text (text_left, line_6_b) (text_right, line_6_t)
            ("rent is " ^ string_of_int v2 ^ " times amount");
          center_text (text_left, line_7_b) (text_right, line_7_t)
            "shown on dice.";
          match Board.get_mortgage sq with
          | Some m ->
              center_text (text_left, line_9_b) (text_right, line_9_t)
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
              center_text (text_left, line_6_b) (text_right, line_6_t)
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
                center_text (text_left, line_8_b) (text_right, line_8_t)
                  ("Mortgage Value $" ^ string_of_int m)
            | None -> ()
          end;
          begin
            match Board.get_buildprice sq with
            | Some n ->
                center_text (text_left, line_9_b) (text_right, line_9_t)
                  ("Houses cost $" ^ string_of_int n ^ " each")
            | None -> ()
          end;
          begin
            match Board.get_buildprice sq with
            | Some n ->
                center_text (text_left, line_10_b)
                  (text_right, line_10_t)
                  ("Hotels, $" ^ string_of_int n ^ " plus 4 houses")
            | None -> ()
          end;
          ()
      | None -> ()
      | _ -> failwith "improper payment structure")
  | None -> ()

let draw_selection_mutables () =
  set_color (rgb 0 0 0);
  let text_0 = calc_sel_b () + calc_sel_buffer () in
  let text_1 = text_0 + calc_sel_line_height () in
  let text_2 = text_1 + calc_sel_line_height () in
  let text_3 = text_2 + calc_sel_line_height () in
  let text_left = calc_sel_l () + calc_sel_buffer () in
  let text_right = calc_sel_l () + calc_sel_w () - calc_sel_buffer () in
  match !sel_state with
  | Some sq -> (
      let i = Board.find_square board sq in
      let owner = State.get_square_owner !game_state i in
      let devlvl = State.get_square_dev_lvl !game_state i in
      let mort = State.get_square_mortgage_state !game_state i in
      begin
        match owner with
        | Some name ->
            center_text (text_left, text_2) (text_right, text_3)
              ("Owner: " ^ name)
        | None -> ()
      end;
      begin
        match devlvl with
        | Some dlvl when dlvl = 0 ->
            center_text (text_left, text_1) (text_right, text_2)
              "Undeveloped"
        | Some dlvl when dlvl = 5 ->
            center_text (text_left, text_1) (text_right, text_2)
              "Development: Hotel"
        | Some dlvl when dlvl = 1 ->
            center_text (text_left, text_1) (text_right, text_2)
              ("Development: " ^ string_of_int dlvl ^ " house")
        | Some dlvl ->
            center_text (text_left, text_1) (text_right, text_2)
              ("Development: " ^ string_of_int dlvl ^ " houses")
        | None -> ()
      end;
      match mort with
      | Some true ->
          center_text (text_left, text_0) (text_right, text_1)
            "Mortgaged"
      | Some false ->
          center_text (text_left, text_0) (text_right, text_1)
            "Not Mortgaged"
      | None -> ())
  | _ -> ()

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
      draw_selection_desc ();
      draw_selection_mutables ()

let draw_player_names_cash_aux elt1 elt2 =
  match (elt1, elt2) with
  | (k1, Some v1), (k2, v2) -> (
      match k1 with
      | 1 ->
          set_color (List.nth Consts.p_colors 0);
          center_text
            (calc_sel_l (), calc_sel_b () + calc_player_name_spacing ())
            ( calc_middle_width (),
              calc_sel_b () + (2 * calc_player_name_spacing ()) )
            (v1 ^ ": $" ^ string_of_int v2)
      | 2 ->
          set_color (List.nth Consts.p_colors 1);
          center_text
            ( calc_middle_width (),
              calc_sel_b () + calc_player_name_spacing () )
            ( calc_sel_l () + calc_sel_w (),
              calc_sel_b () + (2 * calc_player_name_spacing ()) )
            (v1 ^ ": $" ^ string_of_int v2)
      | 3 ->
          set_color (List.nth Consts.p_colors 2);
          center_text
            (calc_sel_l (), calc_sel_b ())
            ( calc_middle_width (),
              calc_sel_b () + calc_player_name_spacing () )
            (v1 ^ ": $" ^ string_of_int v2)
      | 4 ->
          set_color (List.nth Consts.p_colors 3);
          center_text
            (calc_middle_width (), calc_sel_b ())
            ( calc_sel_l () + calc_sel_w (),
              calc_sel_b () + calc_player_name_spacing () )
            (v1 ^ ": $" ^ string_of_int v2)
      | 0 -> ()
      | _ -> failwith "can't draw this many players")
  | _ -> failwith "this is an improperly formatted player name list"

let draw_player_names_cash () =
  set_color (rgb 0 0 0);
  List.iter2 draw_player_names_cash_aux
    (State.get_players_name !game_state)
    (State.get_players_cash !game_state)

let center_circle (x1, y1) (x2, y2) r =
  fill_circle ((x1 + x2) / 2) ((y1 + y2) / 2) r

let draw_player_circle_aux (x1, y1) (x2, y2) i =
  let x_mid = ((x2 - x1) / 2) + x1 in
  let y_mid = ((y2 - y1) / 2) + y1 in
  (match i with
  | 1 -> center_circle (x1, y_mid) (x_mid, y2) Consts.p_token_radius
  | 2 -> center_circle (x_mid, y_mid) (x2, y2) Consts.p_token_radius
  | 3 -> center_circle (x1, y1) (x_mid, y_mid) Consts.p_token_radius
  | 4 -> center_circle (x_mid, y1) (x2, y_mid) Consts.p_token_radius
  | _ -> failwith "improper player to draw the circle");
  if State.get_player_jail_state !game_state i then (
    set_color (rgb 0 0 0);
    match i with
    | 1 -> center_text (x1, y_mid) (x_mid, y2) "J"
    | 2 -> center_text (x_mid, y_mid) (x2, y2) "J"
    | 3 -> center_text (x1, y1) (x_mid, y_mid) "J"
    | 4 -> center_text (x_mid, y1) (x2, y_mid) "J"
    | _ -> failwith "improper player to draw the circle")

let draw_player_circle r i =
  match r.orient with
  | "bot" ->
      draw_player_circle_aux r.lb
        (fst r.rt, snd r.rt - calc_color_h ())
        i
  | "left" ->
      draw_player_circle_aux r.lb
        (fst r.rt - calc_color_h (), snd r.rt)
        i
  | "top" ->
      draw_player_circle_aux
        (fst r.lb, snd r.lb + calc_color_h ())
        r.rt i
  | "right" ->
      draw_player_circle_aux
        (fst r.lb + calc_color_h (), snd r.lb)
        r.rt i
  | "corner" ->
      draw_player_circle_aux
        (fst r.lb + calc_color_h (), snd r.lb + calc_color_h ())
        (fst r.rt - calc_color_h (), snd r.rt - calc_color_h ())
        i
  | _ -> failwith "improper square to draw a player on"

let draw_player_pos_aux msqlst elt =
  match elt with
  | k, v -> (
      match k with
      | 0 -> ()
      | 1 ->
          set_color (List.nth Consts.p_colors 0);
          draw_player_circle (List.nth msqlst v) 1
      | 2 ->
          set_color (List.nth Consts.p_colors 1);
          draw_player_circle (List.nth msqlst v) 2
      | 3 ->
          set_color (List.nth Consts.p_colors 2);
          draw_player_circle (List.nth msqlst v) 3
      | 4 ->
          set_color (List.nth Consts.p_colors 3);
          draw_player_circle (List.nth msqlst v) 4
      | _ -> failwith "improperly formatted player position list")

let draw_player_pos msqlst =
  List.iter
    (draw_player_pos_aux msqlst)
    (State.get_players_position !game_state)

let draw_show_cards () =
  let ll1 =
    ( calc_sel_l (),
      calc_sel_b () + calc_sel_h () - (5 * calc_sel_headline_height ())
    )
  in
  let ur1 =
    ( calc_sel_l () + calc_sel_w (),
      calc_sel_b () + calc_sel_h () - (4 * calc_sel_headline_height ())
    )
  in
  let ll2 =
    ( calc_sel_l (),
      calc_sel_b () + calc_sel_h () - (6 * calc_sel_headline_height ())
    )
  in
  let ur2 =
    ( calc_sel_l () + calc_sel_w (),
      calc_sel_b () + calc_sel_h () - (5 * calc_sel_headline_height ())
    )
  in
  if State.on_cc !game_state !turn_state.dice then (
    set_color (rgb 0 0 0);
    center_text ll1 ur1
      (State.get_cc_pile !game_state |> Cards.name_topcard);
    center_text ll2 ur2
      (State.get_cc_pile !game_state |> Cards.desc_topcard));
  if State.on_chance !game_state !turn_state.dice then (
    set_color (rgb 0 0 0);
    center_text ll1 ur1
      (State.get_chance_pile !game_state |> Cards.name_topcard);
    center_text ll2 ur2
      (State.get_chance_pile !game_state |> Cards.desc_topcard))

let process_roll () =
  let roll = State.roll_dice () in
  let d1, d2 = match roll with v1, v2 -> (v1, v2) in
  let change_turn_state = d1 <> d2 in
  turn_state :=
    {
      !turn_state with
      dice = Some roll;
      has_moved = change_turn_state;
      has_rolled = true;
    };
  draw_show_cards ();
  if change_turn_state then ()
  else
    turn_state :=
      { !turn_state with num_rolls = !turn_state.num_rolls + 1 };
  if !turn_state.num_rolls = 3 then (
    turn_state := { !turn_state with has_moved = true; num_rolls = 0 };
    game_state :=
      State.go_to_jail !game_state (State.current_player !game_state))
  else (
    game_state := State.move !game_state roll;
    if Player.get_jail_state (State.current_player !game_state) > 0 then
      turn_state := { !turn_state with has_moved = true }
    else if State.can_pay_rent !game_state (d1 + d2) then
      game_state := State.add_rent !game_state (d1 + d2)
    else if State.can_pay_luxury !game_state then
      game_state := State.add_luxury_tax !game_state
    else if State.can_pay_income !game_state then
      game_state := State.add_income_tax !game_state
    else ())

let draw_roll_and_turn () =
  set_color (rgb 0 0 0);
  let name =
    match State.current_turn_name !game_state with
    | Some s -> s
    | None -> ""
  in
  center_text
    ( calc_sel_l (),
      calc_sel_b () + calc_sel_h ()
      - ((4 * calc_sel_headline_height ()) - calc_sel_line_height ()) )
    ( calc_sel_l () + calc_sel_w (),
      calc_sel_b () + calc_sel_h ()
      - ((3 * calc_sel_headline_height ()) - calc_sel_line_height ()) )
    ("Current Turn: " ^ name);
  if !turn_state.has_rolled then
    match !turn_state.dice with
    | Some (v1, v2) ->
        center_text
          ( calc_sel_l (),
            calc_sel_b () + calc_sel_h ()
            - (4 * calc_sel_headline_height ()) )
          ( calc_sel_l () + calc_sel_w (),
            calc_sel_b () + calc_sel_h ()
            - (3 * calc_sel_headline_height ()) )
          ("Dice Roll: " ^ string_of_int v1 ^ ", " ^ string_of_int v2)
    | None -> ()

let process_endturn () =
  game_state := State.end_turn !game_state;
  turn_state :=
    {
      !turn_state with
      has_moved = false;
      has_rolled = false;
      has_picked_card = false;
      num_rolls = 0;
    }

let process_prop_purchase () =
  if !turn_state.has_moved then
    game_state := State.buy_property !game_state

let process_payment () =
  if !turn_state.has_rolled then game_state := State.pay !game_state

let process_mortgaging_aux s =
  let ind = Board.find_square board s in
  if State.get_square_mortgage_state !game_state ind = Some true then
    if State.can_unmortgage !game_state ind then
      game_state := State.unmortgage !game_state ind
    else ()
  else if State.get_square_mortgage_state !game_state ind = Some false
  then
    if State.can_mortgage !game_state ind then
      game_state := State.mortgage !game_state ind
    else ()
  else ()

let process_mortgaging () =
  let sq = !sel_state in
  match sq with Some s -> process_mortgaging_aux s | None -> ()

let process_develop_aux s =
  let ind = Board.find_square board s in
  if State.can_develop_property !game_state ind then
    game_state := State.develop_property !game_state ind
  else ()

let process_develop () =
  let sq = !sel_state in
  match sq with Some s -> process_develop_aux s | None -> ()

let process_undevelop_aux s =
  let ind = Board.find_square board s in
  if State.can_undevelop_property !game_state ind then
    game_state := State.undevelop_property !game_state ind
  else ()

let process_undevelop () =
  let sq = !sel_state in
  match sq with Some s -> process_undevelop_aux s | None -> ()

let process_game_over () =
  if State.game_over !game_state then game_over := true

(* game_state := State.move !game_state roll *)

(*********************************************************)
(**** Code that runs everthing - Add functions above ****)
(*********************************************************)

let update1 st msquare_lst =
  if st.key = Consts.demo_key then game_state := State.demo_game_state;
  if
    st.key = Consts.move_key
    && !turn_state.has_moved = false
    && State.current_player !game_state |> Player.no_debt
  then process_roll ();
  if st.key = Consts.end_turn_key && !turn_state.has_moved = true then (
    if not !turn_state.has_picked_card then (
      game_state := State.cards !game_state;
      turn_state := { !turn_state with has_picked_card = true });
    if State.current_player !game_state |> Player.no_debt then
      process_endturn ());
  (* temporary "buy property key" *)
  if st.key = Consts.buy_key && State.can_buy_property !game_state then
    process_prop_purchase ();
  (* temporary "pay rent key" *)
  if st.key = Consts.pay_key then process_payment ();
  if st.key = Consts.mortgage_key then process_mortgaging ();
  if st.key = Consts.develop_key then process_develop ();
  if st.key = Consts.undevelop_key then process_undevelop ()

let update2 st msquare_lst =
  button_handler st;
  update_sel_state st msquare_lst;
  mouseloc_handler (st.mouse_x, st.mouse_y) msquare_lst;
  draw_player_names_cash ();
  draw_roll_and_turn ();
  draw_selection msquare_lst;
  draw_all_colors msquare_lst msquare_color_lst;
  draw_all_msquares msquare_lst;
  draw_player_pos msquare_lst

let update () =
  clear_graph ();
  process_game_over ();
  set_line_width Consts.const_line_width;
  let msquare_lst = construct_msquares () in
  let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
  update1 st msquare_lst;
  update2 st msquare_lst

let driver () =
  while not !game_over do
    update ();
    synchronize ()
  done;
  close_graph ();
  State.winner !game_state

let play_game nms =
  game_state := State.init_game_state nms;
  open_window ();
  let msquare_lst = construct_msquares () in
  draw_all_colors msquare_lst msquare_color_lst;
  draw_all_msquares msquare_lst;
  auto_synchronize false;
  driver ()
