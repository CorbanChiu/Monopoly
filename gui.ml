open Graphics

type coord = int * int

type rect = {
  lb : coord;
  lt : coord;
  rb : coord;
  rt : coord;
  orient : string;
}

let const_window_size = (1100, 800)

let const_buffer = snd const_window_size / 20

let const_valid_height = snd const_window_size - (2 * const_buffer)

let const_valid_width = fst const_window_size - (2 * const_buffer)

let const_board_height = const_valid_height

let const_board_width = const_valid_height

let const_w = const_board_width / 12

let const_h = const_board_height / 9

let const_d = const_h - const_w

let const_l =
  ((const_valid_width - const_board_width) / 2) + const_buffer

let const_r =
  ((const_valid_width - const_board_width) / 2)
  + const_board_width + const_buffer

let const_t = const_valid_height + const_buffer

(* int_of_float ((float_of_int const_t -. float_of_int const_b) /. 11.) *)

(* int_of_float (float_of_int const_w *. 1.5) *)

let const_board_path = "board_monopoly.json"

let name_lst =
  Board.namelist
    (Board.from_json (Yojson.Basic.from_file const_board_path))

let construct_rect (n : int) =
  match n with
  | bottomright when n = 0 ->
      {
        lb = (const_r - const_h, const_buffer);
        lt = (const_r - const_h, const_buffer + const_h);
        rb = (const_r, const_buffer);
        rt = (const_r, const_buffer + const_h);
        orient = "corner";
      }
  | bottom when n < 10 ->
      {
        lb =
          ( const_l + ((10 - (n mod 10)) * const_w) + const_d,
            const_buffer );
        lt =
          ( const_l + ((10 - (n mod 10)) * const_w) + const_d,
            const_buffer + const_h );
        rb =
          ( const_l + ((10 - (n mod 10)) * const_w) + const_w + const_d,
            const_buffer );
        rt =
          ( const_l + ((10 - (n mod 10)) * const_w) + const_w + const_d,
            const_buffer + const_h );
        orient = "bot";
      }
  | bottomleft when n = 10 ->
      {
        lb = (const_l, const_buffer);
        lt = (const_l, const_buffer + const_h);
        rb = (const_l + const_h, const_buffer);
        rt = (const_l + const_h, const_buffer + const_h);
        orient = "corner";
      }
  | left when n < 20 ->
      {
        lb = (const_l, const_buffer + (n mod 10 * const_w) + const_d);
        lt =
          ( const_l,
            const_buffer + (n mod 10 * const_w) + const_w + const_d );
        rb =
          ( const_l + const_h,
            const_buffer + (n mod 10 * const_w) + const_d );
        rt =
          ( const_l + const_h,
            const_buffer + (n mod 10 * const_w) + const_w + const_d );
        orient = "left";
      }
  | topleft when n = 20 ->
      {
        lb = (const_l, const_t - const_h - const_d);
        lt = (const_l, const_t - const_d);
        rb = (const_l + const_h, const_t - const_h - const_d);
        rt = (const_l + const_h, const_t - const_d);
        orient = "corner";
      }
  | top when n < 30 ->
      {
        lb =
          ( const_l + const_d + (n mod 10 * const_w),
            const_t - const_h - const_d );
        lt =
          (const_l + const_d + (n mod 10 * const_w), const_t - const_d);
        rb =
          ( const_l + const_d + (n mod 10 * const_w) + const_w,
            const_t - const_d );
        rt =
          ( const_l + const_d + (n mod 10 * const_w) + const_w,
            const_t - const_d );
        orient = "top";
      }
  | topright when n = 30 ->
      {
        lb = (const_r - const_h, const_t - const_h - const_d);
        lt = (const_r - const_h, const_t - const_d);
        rb = (const_r, const_t - const_h - const_d);
        rt = (const_r, const_t - const_d);
        orient = "corner";
      }
  | right when n < 40 ->
      {
        lb =
          ( const_r - const_h,
            const_t - (n mod 10 * const_w) - const_w - (2 * const_d) );
        lt =
          ( const_r - const_h,
            const_t - (n mod 10 * const_w) - (2 * const_d) );
        rb =
          ( const_r,
            const_t - (n mod 10 * const_w) - const_w - (2 * const_d) );
        rt = (const_r, const_t - (n mod 10 * const_w) - (2 * const_d));
        orient = "right";
      }
  | _ -> failwith "bad shape"

let get_x r = fst r.lb

let get_y r = snd r.lb

let abs_val value = if value < 0 then -value else value

let get_w r = fst r.rb - fst r.lb |> abs_val

let get_h r = snd r.lt - snd r.lb |> abs_val

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
  draw_rect (get_x rect) (get_y rect) (get_w rect) (get_h rect)

let draw_all_rects cds = List.iter draw_one_rect cds

let draw_background =
  open_graph " 1440x750+100-100";
  set_window_title "Monopoly";
  set_line_width 2;
  List.iter draw_one_rect coords
