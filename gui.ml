open Graphics

type coord = int * int

type rect = {
  lb : coord;
  lt : coord;
  rb : coord;
  rt : coord;
  orient : string;
}

let const_window_size = (1100, 700)

let const_buffer = snd const_window_size / 20

let const_valid_height = snd const_window_size - (2 * const_buffer)

let const_valid_width = fst const_window_size - (2 * const_buffer)

let const_board_height = const_valid_height

let const_board_width = const_valid_height

let const_w = const_board_width / 12

let const_h = const_board_height / 8

let const_d = const_h - const_w

let const_l =
  ((const_valid_width - const_board_width) / 2) + const_buffer

let const_r =
  ((const_valid_width - const_board_width) / 2) + const_board_width

let const_t = const_buffer + const_board_height

let const_b = const_buffer

let const_color_h = const_h / 5

let const_board_path = "board_monopoly.json"

let board = Board.from_json (Yojson.Basic.from_file const_board_path)

let name_lst = Board.namelist board

let color_lst = Board.colorlist board

let construct_rect (n : int) =
  match n with
  | bottomright when n = 0 ->
      {
        lb = (const_l + (9 * const_w) + const_h, const_b);
        lt = (const_l + (9 * const_w) + const_h, const_b + const_h);
        rb = (const_l + (9 * const_w) + (2 * const_h), const_b);
        rt = (const_l + (9 * const_w) + (2 * const_h), const_b + const_h);
        orient = "corner";
      }
  | bottom when n < 10 ->
      {
        lb = (const_l + ((9 - (n mod 10)) * const_w) + const_h, const_b);
        lt =
          ( const_l + ((9 - (n mod 10)) * const_w) + const_h,
            const_b + const_h );
        rb =
          ( const_l + ((9 - (n mod 10)) * const_w) + const_w + const_h,
            const_b );
        rt =
          ( const_l + ((9 - (n mod 10)) * const_w) + const_w + const_h,
            const_b + const_h );
        orient = "bot";
      }
  | bottomleft when n = 10 ->
      {
        lb = (const_l, const_b);
        lt = (const_l, const_b + const_h);
        rb = (const_l + const_h, const_b);
        rt = (const_l + const_h, const_b + const_h);
        orient = "corner";
      }
  | left when n < 20 ->
      {
        lb = (const_l, const_b + (n mod 10 * const_w) + const_d);
        lt =
          (const_l, const_b + (n mod 10 * const_w) + const_w + const_d);
        rb =
          (const_l + const_h, const_b + (n mod 10 * const_w) + const_d);
        rt =
          ( const_l + const_h,
            const_b + (n mod 10 * const_w) + const_w + const_d );
        orient = "left";
      }
  | topleft when n = 20 ->
      {
        lb = (const_l, const_b + (9 * const_w) + const_h);
        lt = (const_l, const_b + (9 * const_w) + (2 * const_h));
        rb = (const_l + const_h, const_b + (9 * const_w) + const_h);
        rt = (const_l + const_h, const_b + (9 * const_w) + (2 * const_h));
        orient = "corner";
      }
  | top when n < 30 ->
      {
        lb =
          ( const_l + const_d + (n mod 10 * const_w),
            const_b + (9 * const_w) + const_h );
        lt =
          ( const_l + const_d + (n mod 10 * const_w),
            const_b + (9 * const_w) + (2 * const_h) );
        rb =
          ( const_l + const_d + (n mod 10 * const_w) + const_w,
            const_b + (9 * const_w) + const_h );
        rt =
          ( const_l + const_d + (n mod 10 * const_w) + const_w,
            const_b + (9 * const_w) + (2 * const_h) );
        orient = "top";
      }
  | topright when n = 30 ->
      {
        lb =
          ( const_l + (9 * const_w) + const_h,
            const_b + (9 * const_w) + const_h );
        lt =
          ( const_l + (9 * const_w) + const_h,
            const_b + (9 * const_w) + (2 * const_h) );
        rb =
          ( const_l + (9 * const_w) + (2 * const_h),
            const_b + (9 * const_w) + const_h );
        rt =
          ( const_l + (9 * const_w) + (2 * const_h),
            const_b + (9 * const_w) + (2 * const_h) );
        orient = "corner";
      }
  | right when n < 40 ->
      {
        lb =
          ( const_l + (9 * const_w) + const_h,
            const_b + const_h + ((9 - (n mod 10)) * const_w) );
        lt =
          ( const_l + (9 * const_w) + const_h,
            const_b + const_h + ((10 - (n mod 10)) * const_w) );
        rb =
          ( const_l + (9 * const_w) + (2 * const_h),
            const_b + const_h + ((9 - (n mod 10)) * const_w) );
        rt =
          ( const_l + (9 * const_w) + (2 * const_h),
            const_b + const_h + ((10 - (n mod 10)) * const_w) );
        orient = "right";
      }
  | _ -> failwith "bad shape"

let get_rect_x r = fst r.lb

let get_rect_y r = snd r.lb

let get_rect_w r = fst r.rb - fst r.lb

let get_rect_h r = snd r.lt - snd r.lb

let get_x = fst

let get_y = snd

let coords = List.init 40 construct_rect

(* For debugging *)
(* let print_cd (x, y) = match (x, y) with | x, y -> print_string "(";
   print_int x; print_string ", "; print_int y; print_string ")"

   let rec print_cdlst = function | [] -> () | h :: t -> print_cd h;
   print_string "; "; print_cdlst t

   let get_lb cd = cd.lb

   let get_lt cd = cd.lt

   let get_rb cd = cd.rb

   let get_rt cd = cd.rt

   let print_lb = print_cdlst (List.map get_lb coords)

   let print_lt = print_cdlst (List.map get_lt coords)

   let print_rb = print_cdlst (List.map get_rb coords)

   let print_rt = print_cdlst (List.map get_rt coords) *)

let draw_one_rect rect =
  set_color (rgb 0 0 0);
  draw_rect (get_rect_x rect) (get_rect_y rect) (get_rect_w rect)
    (get_rect_h rect)

let draw_all_rects = List.iter draw_one_rect

let color_area rect =
  match rect with
  | bot when rect.orient = "bot" ->
      Some
        {
          lb = (get_x rect.lt, get_y rect.lt - const_color_h);
          lt = rect.lt;
          rb = (get_x rect.rt, get_y rect.rt - const_color_h);
          rt = rect.rt;
          orient = "color";
        }
  | left when rect.orient = "left" ->
      Some
        {
          lb = (get_x rect.rb - const_color_h, get_y rect.rb);
          lt = (get_x rect.rt - const_color_h, get_y rect.rt);
          rb = rect.rb;
          rt = rect.rt;
          orient = "color";
        }
  | top when rect.orient = "top" ->
      Some
        {
          lb = rect.lb;
          lt = (get_x rect.lb, get_y rect.lb + const_color_h);
          rb = rect.rb;
          rt = (get_x rect.rb, get_y rect.lb + const_color_h);
          orient = "color";
        }
  | right when rect.orient = "right" ->
      Some
        {
          lb = rect.lb;
          lt = rect.lt;
          rb = (get_x rect.lb + const_color_h, get_y rect.lb);
          rt = (get_x rect.lt + const_color_h, get_y rect.lt);
          orient = "color";
        }
  | _ -> None

let draw_one_color rect c =
  match c with
  | Some (r, g, b) -> (
      set_color (rgb r g b);
      match color_area rect with
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

let mouse_handler m : unit =
  match square_hover m coords with
  | Some r ->
      set_color black;
      fill_rect (get_rect_x r) (get_rect_y r) (get_rect_w r)
        (get_rect_h r)
  | None -> ()

(* set_color (rgb 255 255 255); draw_circle (fst m) (snd m) 5 let r =
   square_hover m coords in draw_one_rect r *)
let unsync () =
  clear_graph ();
  draw_all_colors coords color_lst;
  draw_all_rects coords;
  let st = wait_next_event [ Poll ] in
  if st.keypressed then raise Exit;
  mouse_handler (st.mouse_x, st.mouse_y)

let looping () =
  try
    while true do
      auto_synchronize false;
      unsync ();
      synchronize ()
    done
  with Exit -> ()

let draw_background =
  open_graph " 1100x700+100-100";
  set_window_title "Monopoly";
  set_line_width 2;
  draw_all_colors coords color_lst;
  draw_all_rects coords;
  looping ()
