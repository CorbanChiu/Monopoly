(* TODO: appropriately credit this from the Clarkson's textbook *)
(*  *)
(* An ['a pile] is a queue whose elements have type ['a]. *)
type 'a pile

(* The empty pile. *)
val empty : 'a pile

(* Whether a pile is empty. *)
val is_empty : 'a pile -> bool

(* [enqueue x q] is the pile [q] with [x] added to the bottom. *)
val enqueue : 'a -> 'a pile -> 'a pile

(* [peek q] is [Some x], where [x] is the element at the top of the
   pile, or [None] if the pile is empty. *)
val peek : 'a pile -> 'a option

(* [dequeue q] is [Some q'], where [q'] is the queue containing all the
   elements of [q] except the top of [q], or [None] if [q] is empty. *)
val dequeue : 'a pile -> 'a pile option
