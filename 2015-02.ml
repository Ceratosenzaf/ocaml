module type IntervalI =
  sig
    type interval
    type endpoint
    val create : endpoint -> endpoint -> interval
    val is_empty : interval -> bool
    val contains : interval -> endpoint -> bool
    val intersect : interval -> interval -> interval
    val to_string : interval -> string
    exception WrongInterval
  end;;

module type Comparable =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end;;

module Interval (T: Comparable): (IntervalI with type endpoint = T.t) =
  struct
    type endpoint = T.t
    type interval = endpoint * endpoint
    exception WrongInterval

    let create a b = 
      if T.compare a b > 0 then raise WrongInterval
      else a, b

    let is_empty i = T.compare (fst i) (snd i) = 0

    let contains i v = T.compare (fst i) v <= 0 && T.compare (snd i) v >= 0

    let intersect i j = 
      let compare_start = T.compare (fst i) (fst j) in
      let compare_end = T.compare (snd i) (snd j) in
      let a = if compare_start > 0 then (fst j) else (fst i) in
      let b = if compare_end < 0 then (snd j) else (snd i) in
      create a b

    let to_string i = Printf.sprintf "[%s, %s]" (T.to_string (fst i)) (T.to_string (snd i))
  end;;

module IntComparable: (Comparable with type t = int) = 
  struct
    type t = int
    let compare = Int.compare
    let to_string = string_of_int
  end;;

module StringComparable: (Comparable with type t = string) =
  struct
    type t = string
    let compare = String.compare
    let to_string x = x
  end;;

module IntInterval = Interval(IntComparable);;
module StringInterval = Interval(StringComparable);;
