(* merge sort *)
let rec merge a b = match (a, b) with
| ([], _) -> b
| (_, []) -> a
| (h1::t1, h2::t2) -> if h1 < h2 then h1::(merge t1 b) else h2::(merge a t2);;

let rec merge_sort = function
| [] -> []
| [x] -> [x]
| lst -> 
  let n = List.length lst / 2 in
  let l = List.filteri (fun i _ -> i < n) lst in
  let r = List.filteri (fun i _ -> i >= n) lst in
  merge (merge_sort l) (merge_sort r);;

(* quick sort *)
let rec quick_sort = function
| [] -> []
| [x] -> [x]
| hd :: tl ->
  let l = List.filter (fun x -> x < hd) tl in
  let r = List.filter (fun x -> x >= hd) tl in
  (quick_sort l) @ [hd] @ (quick_sort r);;

(* selection sort *)
let rec min = function 
| [] -> None
| [x] -> Some x
| hd :: tl -> 
  let tl_min = min tl in
  match tl_min with
  | None -> Some hd
  | Some x -> if hd <= x then Some hd else Some x;;

let rec remove_once x = function
| [] -> []
| hd :: tl -> if x = hd then tl else hd :: remove_once x tl;;

let rec selection_sort l = 
  let l_min = min l in
  match l_min with
  | None -> []
  | Some x -> x :: selection_sort (remove_once x l);;

(* insertion sort *)
let rec insert_sorted x = function
| [] -> [x]
| hd :: tl -> if x > hd then hd :: insert_sorted x tl else x :: hd :: tl;;

let rec insertion_sort ?(sorted=[]) = function
  | [] -> sorted
  | hd :: tl -> insertion_sort ~sorted:(insert_sorted hd sorted) tl;;