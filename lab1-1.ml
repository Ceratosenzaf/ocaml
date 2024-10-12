(* 1 *)
let alkaline_earth_metals = [4;20;12;38;56;88];;

let rec highest = function
| [] -> failwith "empty" 
| [x] -> x
| h::t -> 
    let h2 = highest t in
    if h>=h2 then h else h2;;

highest alkaline_earth_metals;;

(* 2 *)
let rec remove x = function
| [] -> []
| h::t ->
    if h=x then t else h::(remove x t);;
 
let rec sort = function
| [] -> []
| [x] -> [x]
| l ->
    let m = highest l in
    let l2 = remove m l in
    (sort l2)@[m];;

sort alkaline_earth_metals;;

(* 3 *)
let noble_gases = [2;18;10;36;54;86];;

let merge a b =
    let x = sort a in
    let y = sort b in
    let rec inner_merge a b =
        match (a, b) with
        | [], l -> l
        | l, [] -> l
        | h1::t1, h2::t2 ->
            if h1 <= h2 then h1::(inner_merge t1 (h2::t2))
            else h2::(inner_merge (h1::t1) t2)
    in inner_merge x y;;

merge alkaline_earth_metals noble_gases;;