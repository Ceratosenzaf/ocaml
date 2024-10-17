module type MonoidADT = sig
  type 'a m
  val op: 'a m -> 'a m -> 'a m
  val i: 'a m
end
;;

module BoolMonoid: MonoidADT = struct
  type 'a m = bool
  let op a b = a || b
  let i = false
end
;;

module ZnMonoid (T: sig val n: int end): MonoidADT = struct
  type 'a m = int
  let op a b = (a + b) mod T.n
  let i = 0
end
