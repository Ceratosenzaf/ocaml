module type QueueADT =
sig
  type value
  type node = Empty | Node of value * node
  exception EmptyQueueException
  val empty: unit -> node
  val enqueue: node -> value -> unit
  val dequeue: node -> value
end
;;

module GenericQueue (T: sig type t end) =
struct
  type value = T.t
  type node = Empty | Node of value * node
  exception EmptyQueueException
  let empty () = Empty
  let rec enqueue q v = match q with
  | Empty -> Node (v, Empty)
  | Node (x, next) -> Node (x, enqueue next v)

  let rec dequeue q = match q with
  | Empty -> raise EmptyQueueException
  | Node (v, next) -> (v, next)
end
;;

module IntQueue = GenericQueue(struct type t = int end);;
let q = IntQueue.empty ();;
let q = IntQueue.enqueue q 1;;
let q = IntQueue.enqueue q 2;;
let (_, q) = IntQueue.dequeue q;;