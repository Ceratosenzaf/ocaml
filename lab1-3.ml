(* 1 *)
let zeroes n m =
  let rec cols l =
    if l <= 0 then []
    else 0::(cols (l-1)) in
  let rec rows l =
    if l <= 0 then []
    else (cols m)::(rows (l-1))
  in rows n 
;;

(* 2 *)
let identity n =
  let rec rows i =
    if i >= n then []
    else
      let rec cols j =
        if j >= n then []
        else if i = j then 1 :: cols (j + 1)
        else 0 :: cols (j + 1)
      in (cols 0) :: rows (i + 1)
  in rows 0
;;

(* 3 *)
let init n = 
  let rec rows i =
    if i >= n then []
    else 
      let rec cols j = 
        if j >= n then []
        else (i*n + j)::cols(j+1)
    in cols(0)::rows(i+1)
  in rows 0
;;

(* 4 *)
let transpose m =
  let rows = Array.length m in
  let cols = Array.length m.(0) in
  Array.init cols (fun i ->
    Array.init rows (fun j ->
      m.(j).(i)
    )
  )
;;

let rec transpose = function
  | [] | [] :: _ -> [] 
  | m ->
    let first_column = List.map List.hd m in
    let rest = List.map List.tl m in
    first_column :: transpose rest
;;

