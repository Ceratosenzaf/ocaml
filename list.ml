let rec length = function [] -> 0 | _ :: tl -> 1 + length tl;;
let rec compare_lengths x y = match (x,y) with
| ([], []) -> 0
| ([], _) -> -1
| (_, []) -> 1
| (_ :: t1, _ :: t2) -> compare_lengths t1 t2;;

let compare_length_with x n =
  let rec compare_lengths_with i = function
  | [] -> compare i n
  | hd :: tl -> 
    if i > n then -1 else compare_lengths_with (i+1) tl
  in compare_lengths_with 0 x;;

let is_empty l = l = [];;

let cons x l = x :: l;;

let hd = function [] -> failwith "hd" | hd :: _ -> hd;;

let tl = function [] -> failwith "tl" | _ :: tl -> tl;;

let nth l n =
  if n < 0 then failwith "nth"
  else let rec nth i = function
  | [] -> failwith "nth"
  | hd :: tl -> if i = n then hd else nth (i+1) tl
  in nth 0 l;;

let nth_opt l n =
  if n < 0 then None
  else let rec nth_opt i = function
  | [] -> None
  | hd :: tl -> if i = n then Some hd else nth_opt (i+1) tl
  in nth_opt 0 l;;

let rec rev_append a b = match a with
| [] -> b 
| hd :: tl -> rev_append tl (hd :: b);;

let rec rev l = rev_append l [];;

let init n f =
  let rec init i =
    if i = n then []
    else f i :: init (i+1)
  in init 0;;

let append = (@);;

let rec flatten = function
| [] -> []
| hd :: tl -> hd @ flatten tl;;

let rec equal f a b = match (a,b) with
| ([], []) -> true
| ([], _) | (_, []) -> false
| (h1 :: t1, h2 :: t2) -> f h1 h2 && equal f t1 t2;;

let rec compare f a b = match (a,b) with
| ([], []) -> 0
| ([], _) -> -1
| (_, []) -> 1
| (h1 :: t1, h2 :: t2) ->
  let v = f h1 h2 in
  if v <> 0 then v
  else compare f t1 t2;;

let rec iter f = function
| [] -> ()
| hd :: tl -> f hd; iter f tl;;

let iteri f l =
  let rec iteri i = function
  | [] -> ()
  | hd :: tl -> f i hd; iteri (i+1) tl
  in iteri 0 l;;

let rec map f = function
| [] -> []
| hd :: tl -> f hd :: map f tl;;

let mapi f l =
  let rec mapi i = function
  | [] -> []
  | hd :: tl -> f i hd :: mapi (i+1) tl
  in mapi 0 l;;

let rev_map f l = 
  let rec rev_map acc = function
  | [] -> acc
  | hd :: tl -> rev_map (f hd :: acc) tl
  in rev_map [] l;;

let rec filtered_map f = function
| [] -> []
| hd :: tl ->
  let rest = filtered_map f tl
  in match f hd with
  | Some v -> v :: rest
  | None -> rest;;

let rec concat_map f = function
| [] -> []
| hd :: tl -> f hd @ concat_map f tl;;

let rec fold_left f acc = function
| [] -> acc
| hd :: tl -> fold_left f (f acc hd) tl;; 

let rec fold_right f acc = function
| [] -> acc
| hd :: tl -> f hd (fold_right f acc tl);;

let rec iter2 f a b = match (a,b) with
| ([], []) -> ()
| ([], _) | (_, []) -> failwith "iter2"
| (h1 :: t1, h2 :: t2) -> f h1 h2; iter2 f t1 t2;;

let rec map2 f a b = match (a,b) with
| ([], []) -> []
| ([], _) | (_, []) -> failwith "map2"
| (h1 :: t1, h2 :: t2) -> f h1 h2 :: map2 f t1 t2;;

let rev_map2 f a b =
  let rec rev_map2 acc a b = match (a,b) with
  | ([], []) -> acc
  | ([], _) | (_, []) -> failwith "rev_map2"
  | (h1 :: t1, h2 :: t2) -> rev_map2 (f h1 h2 :: acc) t1 t2
  in rev_map2 [] a b;;

 
let for_all f l = fold_left (&&) true (map f l);;
let rec for_all f = function
| [] -> true
| hd :: tl -> f hd && for_all f tl;;

let exists f l = fold_left (||) false (map f l);;
let rec exists f = function
| [] -> false
| hd :: tl -> f hd || exists f tl;;

let rec mem v = function
| [] -> false
| hd :: tl -> v = hd || mem v tl;;

let rec find f = function
| [] -> failwith "find"
| hd :: tl -> if f hd then hd else find f tl;;

let find_index f l =
  let rec find_index i = function
  | [] -> failwith "find_index"
  | hd :: tl -> if f hd then i else find_index (i+1) tl
  in find_index 0 l;;

let rec filter f = function
| [] -> []
| hd :: tl -> 
  let rest = filter f tl
  in if f hd then hd :: rest else rest;;

let filteri f l =
  let rec filteri i = function
  | [] -> []
  | hd :: tl ->
    let rest = filteri (i+1) tl
    in if f i hd then hd :: rest else rest
  in filteri 0 l;;

let partition f l =
  let rec partition yes no = function
  | [] -> (rev yes, rev no)
  | hd :: tl -> if f hd then partition (hd :: yes) no tl else partition yes (hd :: no) tl
  in partition [] [] l;;

let rec assoc v = function
| [] -> failwith "assoc"
| (a,b) :: tl -> if v = a then b else assoc v tl;;

let rec remove_assoc v = function
| [] -> []
| (a,b as hd) :: tl -> if v = a then tl else hd :: remove_assoc v tl;;

let split l = 
  let rec split a b = function
  | [] -> (rev a, rev b)
  | (h1, h2) :: tl -> split (h1 :: a) (h2 :: b) tl
  in split [] [] l;; 

let rec combine a b = match (a,b) with
| ([], []) -> []
| ([], _) | (_, []) -> failwith "combine"
| (h1 :: t1, h2 :: t2) -> (h1,h2) :: combine t1 t2;;

let rec merge a b = match (a,b) with
| ([], []) -> []
| ([], _) -> b
| (_, []) -> a
| (h1 :: t1, h2 :: t2) ->
  if h1 <= h2 then h1 :: merge t1 b
  else h2 :: merge a t2;;

let rec splitn lst n =
  if n = 0 then ([], lst)
  else match lst with
  | [] -> ([], [])
  | hd :: tl ->
    let (l, r) = splitn tl (n-1) in
    (hd :: l, r);;

let rec merge_sort = function
| [] -> []
| [x] -> [x]
| lst ->
  let m = length lst / 2 in
  let (l, r) = splitn lst m in
  merge (merge_sort l) (merge_sort r);;

let of_string s = 
  let rec of_string i = 
    if i = String.length s then []
    else s.[i] :: of_string (i+1)
  in of_string 0;; 