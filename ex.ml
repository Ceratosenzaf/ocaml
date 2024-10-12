(* 1 *)
let rec last = function 
  | [] -> None 
  | [ x ] -> Some x 
  | hd :: tl -> last tl

(* 2 *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl

(* 3 *)
let rec nth lst i =
  match lst with
  | [] -> None
  | hd :: tl when i == 0 -> Some hd
  | hd :: tl -> nth tl (i - 1)

(* 4 *)
let len lst =
  let rec inner_len = function 
  | [] -> 0 
  | hd :: tl -> 1 + inner_len tl in
  inner_len lst

(* 5 *)
let rec rev = function
  | [] -> []
  | [ x; y ] -> [ y; x ]
  | hd :: tl -> rev tl @ [ hd ]

(* 6 *)
let is_palindrome l = l = rev l

(* 7 *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lst = 
  let rec value_of = function
  | [] -> []
  | One x :: tl -> x :: value_of tl
  | Many x :: tl -> value_of x @ value_of tl 
  in value_of lst;;

(* 8 *)
let rec compress = function
| [] -> []
| [x] -> [x]
| h1 :: (h2 :: _ as tl) -> 
  if h1 = h2 then compress tl
  else h1 :: compress tl;;

(* 9 *)
let pack lst = 
  let rec inner_pack curr acc = function
  | [] -> []
  | [x] -> (x :: curr) :: acc
  | h1 :: (h2 :: _ as tl) ->
    let new_curr = h1 :: curr in
    if h1 = h2 then inner_pack new_curr acc tl
    else inner_pack [] (new_curr :: acc) tl
  in rev (inner_pack [] [] lst);; 

(* 10 *)
let encode lst =
  let rec inner_encode curr = function
  | [] -> []
  | [x] -> [(curr, x)]
  | h1 :: (h2 :: _ as tl) ->
    let new_curr = curr + 1 in
    if h1 = h2 then inner_encode new_curr tl
    else (new_curr, h1) :: inner_encode 0 tl 
  in inner_encode 0 lst;;

(* 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let create_tuple n v =
    if n <= 1 then One v
    else Many (n, v) 
  in let rec inner_encode curr = function
  | [] -> []
  | [x] -> [create_tuple curr x]
  | h1 :: (h2 :: _ as tl) ->
    let new_curr = curr + 1 in
    if h1 = h2 then inner_encode new_curr tl
    else (create_tuple new_curr h1) :: inner_encode 0 tl 
  in inner_encode 0 lst;;

(* 12 *)
let rec decode = function
  | [] -> []
  | One x :: tl -> x :: decode tl
  | Many (n, v) :: tl when n > 0 -> (v :: decode [Many ((n-1), v)]) @ decode tl
  | _ -> [];;

let decode lst = 
  let rec many n v =
    if n = 0 then []
    else v :: many (n-1) v 
  in let rec inner_decode = function
  | [] -> []
  | One x :: tl -> x :: inner_decode tl
  | Many (n, v) :: tl -> many n v @ inner_decode tl
  in inner_decode lst;;

(* 13 *)
let rec duplicate = function
| [] -> []
| hd :: tl -> hd :: hd :: (duplicate tl);;

(* 14 *)
let replicate lst n = 
  let rec repeat n v =
    if n <= 0 then []
    else v :: repeat (n-1) v
  in let rec inner_replicate = function
  | [] -> []
  | hd :: tl -> repeat n hd @ inner_replicate tl
  in inner_replicate lst;;

(* 15 *)
let drop lst n =
  let rec inner_drop i = function
  | [] -> []
  | hd :: tl -> 
    if i <= 1 then inner_drop n tl
    else hd :: inner_drop (i-1) tl
  in inner_drop n lst;;

(* 16 *)
let split lst n =
  let rec inner_split i acc1 acc2 = function
  | [] -> (rev acc1, rev acc2)
  | hd :: tl -> 
    if i < n then inner_split (i+1) (hd::acc1) acc2 tl
    else inner_split (i+1) acc1 (hd::acc2) tl
  in inner_split 0 [] [] lst;;

(* 17 *)
let extract lst a b =
  let rec inner_extract i = function
  | [] -> []
  | hd :: tl -> 
    if i >= a && i <= b then hd :: inner_extract (i+1) tl
    else inner_extract (i+1) tl
  in inner_extract 0 lst;;

(* 18 *)
let rotate lst n = 
  let rec inner_rotate i acc = function
  | [] -> acc
  | hd :: tl ->
    if i >= n then hd :: inner_rotate (i+1) acc tl
    else inner_rotate (i+1) (acc@[hd]) tl
  in inner_rotate 0 [] lst;;

(* 19 *)
let rec remove_at i = function
| [] -> []
| hd :: tl ->
  let rest = remove_at (i-1) tl in
  if i = 0 then rest else hd :: rest;;

(* 20 *)
let rec insert_at v i = function
| [] -> [v]
| hd :: tl -> 
  if i = 0 then v :: hd :: tl
  else hd :: insert_at v (i-1) tl;;

(* 21 *)
let range a b =
  let rec inner_range a b =
    if b >= a then a :: inner_range (a+1) b
    else []
  in if b >= a then inner_range a b else rev (inner_range b a);;

(* 22 *)
let rec extract n = function
  | _ when n <= 0 -> [[]]
  | [] -> []
  | hd :: tl -> 
    let with_hd = List.map (fun e -> hd :: e) (extract (n-1) tl)
    in let without_hd = extract n tl
    in with_hd @ without_hd;;

(* 22 *)
let rec drop_value v = function
  | [] -> []
  | hd :: tl -> 
    if hd = v then tl
    else hd :: drop_value v tl;;

let rec contains v = function
  | [] -> false
  | hd :: tl ->
      if hd = v then true
      else contains v tl;;

let rec drop_values values = function
  | [] -> []
  | hd :: tl ->
    let rest = drop_values values tl in
    if contains hd values then rest
    else hd :: rest;;

let rec drop_values_once values = function
  | [] -> []
  | hd :: tl ->
    if contains hd values then drop_values_once (drop_value hd values) tl
    else hd :: drop_values_once values tl;;

let rec group lst sizes =
  if len lst <> List.fold_left (+) 0 sizes then [[]]
  else match sizes with
  | [] -> [[]]
  | hd :: tl ->
    if hd <= 0 then group lst tl
    else if hd > len lst then [[]]
      else let single_groups = extract hd lst in
      List.concat(
        List.map(fun single_group -> 
          let remaining_elements = drop_values_once single_group lst in
          let remaining_groups = group remaining_elements tl in
          List.map(fun group -> single_group :: group) remaining_groups 
        ) single_groups
      );;
