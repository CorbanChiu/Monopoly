(* TODO: appropriately credit this from the Clarkson's textbook *)
(* *)
(* [{front=[a;b]; back=[e;d;c]}] represents the queue containing the
   elements a,b,c,d,e. That is, the back of the queue is stored in
   reverse order. [{front; back}] is in *normal form* if [front] being
   empty implies [back] is also empty. All queues passed into or out of
   the module must be in normal form. *)
type 'a pile = {
  front : 'a list;
  back : 'a list;
}

let empty = { front = []; back = [] }

let is_empty = function { front = []; back = [] } -> true | _ -> false

(* Helper function to ensure that a queue is in normal form. *)
let norm = function
  | { front = []; back } -> { front = List.rev back; back = [] }
  | q -> q

let enqueue x q = norm { q with back = x :: q.back }

let peek = function
  | { front = []; _ } -> None
  | { front = x :: _; _ } -> Some x

let dequeue = function
  | { front = []; _ } -> None
  | { front = _ :: xs; back } -> Some (norm { front = xs; back })
