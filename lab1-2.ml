(* 1 *)
type scale = C | F | K | Ra | D | N | Re | Ro ;;
type temp = { value: float; unit: scale } ;;

let toC t = match t.unit with
| C -> t
| F -> { unit = C; value = t.value -. 32. *. 5. /. 9. }
| K -> { unit = C; value = t.value -. 273.15 }
| Ra-> { unit = C; value = t.value -. 491.67 *. 5. /. 9. }
| D -> { unit = C; value = 100. -. t.value *. 2. /. 3. }
| N -> { unit = C; value = t.value *. 100. /. 33. }
| Re-> { unit = C; value = t.value *. 5. /. 4. }
| Ro-> { unit = C; value = t.value -. 7.5 *. 40. /. 21. }
;;

(* 2 *)
let fromC t u = match u with
| C -> t
| F -> { unit = u; value = t.value *. 9. /. 5. +. 32. }
| K -> { unit = u; value = t.value +. 273.15 }
| Ra-> { unit = u; value = t.value  *. 9. /. 5. +. 491.67 }
| D -> { unit = u; value = (100. -. t.value) *. 3. /. 2. }
| N -> { unit = u; value = t.value *. 33. /. 100. }
| Re-> { unit = u; value = t.value *. 4. /. 5. }
| Ro-> { unit = u; value = t.value  *. 21. /. 40. +. 7.5 }
;;

let print t = match t.unit with
| C -> Printf.printf "%.2f° C\n" t.value
| F -> Printf.printf "%.2f° F\n" t.value
| K -> Printf.printf "%.2f° K\n" t.value
| Ra-> Printf.printf "%.2f° Ra\n" t.value
| D -> Printf.printf "%.2f° D\n" t.value
| N -> Printf.printf "%.2f° N\n" t.value
| Re-> Printf.printf "%.2f° Re\n" t.value
| Ro-> Printf.printf "%.2f° Ro\n" t.value
;;

let u = [C; F; K; Ra; D; N; Re; Ro];;
let v = 0.;;

let converted = List.map (fromC { value=v; unit=C }) u in
List.iter print converted;;