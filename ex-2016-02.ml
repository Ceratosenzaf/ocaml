let string_to_list_of_chars s = 
  List.init (String.length s) (String.get s);;

let is_operator c = c = '+' || c = '-' || c = '*' || c = '/';;

let get_operator c = 
  if c = '+' then (+.)
  else if c = '-' then (-.)
  else if c = '*' then ( *. )
  else if c = '/' then (/.)
  else failwith "not an operator";;

let get_operand c =
  let s = String.make 1 c in
  let i = int_of_string s in
  float_of_int i;;

type evaluation = Operator of (float -> float -> float) | Operand of float

let expression_to_evaluation_list e = 
  let l = string_to_list_of_chars e in
  List.map (fun c ->
    if is_operator c
      then Operator (get_operator c)
      else Operand (get_operand c)
  ) l

let rec evaluate = function
| [] -> []
| Operator op :: Operand a :: Operand b :: tl ->
  op a b :: evaluate tl
| Operator op :: Operand a :: tl -> 
  let rest = evaluate tl in
  let b = List.hd rest in
  op a b :: List.tl rest
| Operator op :: tl ->
  let rest = evaluate tl in
  let a = List.hd rest in
  let b = List.hd (List.tl rest) in
  op a b :: List.tl (List.tl rest)
| _ -> failwith "invalid expression";;
   
let evaluation l = List.hd (evaluate l);;
