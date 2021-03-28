type coord

val draw_horizontal_rect : int -> int -> unit

val draw_vertical_rect : int -> int -> unit

val draw_sqlist : ('a -> 'b -> 'c) -> 'b -> 'a list -> unit

val draw_board : unit
