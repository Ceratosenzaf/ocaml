module type StackADT =
sig
  type stack
  exception EmptyStackException
  val empty: unit -> stack
  val push: stack -> int -> unit
  val pop: stack -> unit
  val top: stack -> int
  val is_empty: stack -> bool
end
;;

module IntStack = 
struct
  type stack = { mutable x: int list }
  exception EmptyStackException
  let empty () = { x = [] }
  let push s v = s.x <- v :: s.x
  let pop s = match s.x with [] -> raise EmptyStackException | hd :: tl -> s.x <- tl
  let top s = match s.x with [] -> raise EmptyStackException | hd :: _ -> hd
  let is_empty s = s.x = []
end
;;

let s = IntStack.empty();;
IntStack.push s 3;;
IntStack.push s 5;;
IntStack.pop s;;
IntStack.top s;;
IntStack.is_empty s;;

module GenericStack (T: sig type t end) =
struct
  type stack = { mutable x: T.t list }
  exception EmptyStackException
  let empty () = { x = [] }
  let push s v = s.x <- v :: s.x
  let pop s = match s.x with [] -> raise EmptyStackException | hd :: tl -> s.x <- tl
  let top s = match s.x with [] -> raise EmptyStackException | hd :: _ -> hd
  let is_empty s = s.x = []
end
;;

module IntStack = GenericStack(struct type t = int end);;
module CharStack = GenericStack(struct type t = char end);;

let s1 = IntStack.empty();;
IntStack.push s1 1;;
IntStack.push s1 2;;
IntStack.pop s1;;
IntStack.top s1;;
IntStack.is_empty s1;;


let s2 = CharStack.empty();;
CharStack.push s2 'a';;
CharStack.push s2 'b';;
CharStack.pop s2;;
CharStack.top s2;;
CharStack.is_empty s2;;