let read_lines name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []
;;

let filter_chars str chars_to_remove =
  let len = String.length str in
  let buffer = Buffer.create len in
  for i = 0 to len - 1 do
    let ch = str.[i] in
    if not (String.contains chars_to_remove ch) then
      Buffer.add_char buffer ch
  done;
  Buffer.contents buffer
;;

let filtered_words = 
  let filename = "lab1-4.txt" in
  let lines = read_lines filename in
  let text = String.concat " " lines in
  let lowercase_text = String.lowercase_ascii text in
  let sanitized_text = filter_chars lowercase_text "\".,;:()-?!{}[]" in
  let words = String.split_on_char ' ' sanitized_text in
  List.filter (fun w -> w <> "") words
;;

let frequencies lst =
  let get_v_from_opt opt = match opt with None -> 0 | Some v -> snd v in
  let rec freq acc = function
  | [] -> acc
  | hd :: tl -> 
    let w_in_acc = List.find_opt (fun v -> fst v = hd) acc in
    let new_w_v = (get_v_from_opt w_in_acc) + 1  in
    let new_w_acc = (hd, new_w_v) in
    let acc_without_w = List.filter (fun v -> fst v <> hd) acc in
    freq (new_w_acc :: acc_without_w) tl
  in freq [] lst
;;

let sorted_freq = 
  let words_freq = frequencies filtered_words
  in List.sort (fun x y -> compare (snd x) (snd y) ) words_freq
;;

List.iter (fun v -> Printf.printf "%s: %d\n" (fst v) (snd v) ) sorted_freq;;
    