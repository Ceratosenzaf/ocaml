let is_palyndrome str =
  let l = List.init (String.length str) (String.get str) in
  l = List.rev l;;

let (-) a b =
  let l = List.init (String.length a) (String.get a) in
  let rec remove = function
  | [] -> []
  | hd :: tl -> 
    let has_hd = String.contains b hd in
    if has_hd then remove tl else hd :: remove tl
  in let removed = remove l in
  let buff = Buffer.create (List.length removed) in
  List.iter (Buffer.add_char buff) removed;
  Buffer.contents buff;; 

let anagram s l =
  let chars_of_s = List.init (String.length s) (String.get s) in
  let sorted_chars_of_s = List.sort compare chars_of_s in
  let is_anagram str =
    let chars = List.init (String.length str) (String.get str) in
    let sorted_list_of_str = List.sort compare chars in
    sorted_chars_of_s = sorted_list_of_str
  in List.exists is_anagram l;;
