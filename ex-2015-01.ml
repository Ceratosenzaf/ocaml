module type NaturalI =
 sig
 type natural
 exception NegativeNumber
 exception DivisionByZero
 val ( + ) : natural -> natural -> natural
 val ( - ) : natural -> natural -> natural
 val ( * ) : natural -> natural -> natural
 val ( / ) : natural -> natural -> natural
 val eval : natural -> int
 val convert : int -> natural
 end;;
 
module Natural: NaturalI = 
struct
  type natural = Zero | Succ of natural
  exception NegativeNumber
  exception DivisionByZero

  let rec ( > ) a b = match (a, b) with
  | (Zero, _) -> false
  | (_, Zero) -> true
  | (Succ x, Succ y) -> x > y

  let rec ( + ) a b = match b with
  | Zero -> a
  | Succ x -> Succ (a + x)

  let rec ( - ) a b = match (a, b) with
  | (_, Zero) -> a
  | (Zero, _) -> raise NegativeNumber
  | (Succ x, Succ y) -> x - y

  let rec ( * ) a b = match (a, b) with
  | (_, Zero) | (Zero, _) -> Zero
  | (_, Succ y) -> a + (a * y) 

  let rec ( / ) a b = match (a, b) with
  | (_, Zero) -> raise DivisionByZero
  | (Zero, _) -> Zero
  | (_, _) -> 
    if a = b || (a > b)
    then Succ ((a - b) / b)
    else Zero

  let rec convert = function
  | 0 -> Zero
  | x when x < 0 -> raise NegativeNumber
  | x-> Succ (convert (pred x))

  let rec eval = function
  | Zero -> 0
  | Succ x -> succ (eval x)
end
;;

open Natural;;