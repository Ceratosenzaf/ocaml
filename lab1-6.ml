let rec factorial x = match x with
| 0 -> 1
| _ -> x * factorial (x - 1);;

let rec sin x n = match n with
| 0 -> x
| _ ->
  let y = 2 * n + 1 in
  let z = factorial y in
  let w = x ** float_of_int y in
  let v = w  /. float_of_int z in
  let rest = sin x (n - 1) in
  if n mod 2 = 0 then rest +. v
  else rest -. v;;

let rec cos x n = match n with
| 0 -> 1.
| _ ->
  let y = 2 * n in
  let z = factorial y in
  let w = x ** float_of_int y in
  let v = w  /. float_of_int z in
  let rest = cos x (n - 1) in
  if n mod 2 = 0 then rest +. v
  else rest -. v;;
