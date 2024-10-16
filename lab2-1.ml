module type GraphADT = sig
  type 'a graph

  exception GraphIsEmpty
  exception NodeNotFound
  
  val empty: unit -> 'a graph
  val add_node: 'a graph -> 'a -> 'a graph
  val remove_node: 'a graph -> 'a -> 'a graph
  val add_arc: 'a graph -> 'a -> 'a -> 'a graph
  val remove_arc: 'a graph -> 'a -> 'a -> 'a graph
  val get_nodes: 'a graph -> 'a list
  val get_arcs: 'a graph -> ('a * 'a) list
  val adj: 'a graph -> 'a -> 'a list 
end
;;

module Graph: GraphADT = struct
  type 'a graph = 'a list * ('a * 'a) list
  
  exception GraphIsEmpty
  exception NodeNotFound

  let empty () = ([], [])

  let add_node g v = 
    let contains = List.exists (fun x -> x = v) (fst g) in 
    if contains then g else (v :: (fst g), snd g)
    
  let remove_node g v = 
    let nodes = List.filter (fun x -> x <> v) (fst g) in
    let arcs = List.filter (fun x -> fst x <> v && snd x <> v) (snd g) in
    (nodes, arcs)

  let add_arc g a b = 
    let g' = add_node (add_node g a) b in
    let contains = List.exists (fun x -> x = (a, b)) (snd g') in
    if contains then g' else (fst g', (a, b) :: (snd g')) 
  
  let remove_arc g a b =
    let arcs = List.filter (fun x -> x <> (a, b)) (snd g) in
    (fst g, arcs)

  let get_nodes g = fst g

  let get_arcs g = snd g

  let adj g v = 
    let arcs = List.filter (fun x -> fst x = v) (snd g) in
    List.map (fun x -> snd x) arcs
  
end
;;

let dfs g v = 
  if (Graph.get_nodes g) = [] then raise Graph.GraphIsEmpty
  else if not (List.exists (fun x -> x = v) (Graph.get_nodes g)) then raise Graph.NodeNotFound
  else let rec aux x visited = function
  | [] -> visited
  | hd :: tl when (List.exists (fun x -> x = hd) visited) -> aux x visited tl
  | hd :: tl ->
    let inner = aux hd (hd :: visited) (Graph.adj g hd) in
    aux x inner tl
  in let res = aux v [v] (Graph.adj g v)
  in List.rev res
;;

let bfs g v = 
  if (Graph.get_nodes g) = [] then raise Graph.GraphIsEmpty
  else if not (List.exists (fun x -> x = v) (Graph.get_nodes g)) then raise Graph.NodeNotFound
  else let rec aux x visited = function
  | [] -> visited
  | hd :: tl when (List.exists (fun x -> x = hd) visited) -> aux x visited tl
  | hd :: tl ->
    let rest = aux x (hd :: visited) tl in
    aux hd rest (Graph.adj g hd)
  in let res = aux v [v] (Graph.adj g v)
  in List.rev res
;;

let g = Graph.empty ();;
let g = Graph.add_node g 1;;
let g = Graph.add_node g 2;;
let g = Graph.add_node g 3;;
let g = Graph.add_node g 3;;
let g = Graph.add_node g 4;;
let g = Graph.remove_node g 4;;
let g = Graph.add_arc g 1 2;;
let g = Graph.add_arc g 1 3;;
let g = Graph.add_arc g 2 3;;
let g = Graph.add_arc g 3 5;;

let print_list l = List.iter (fun x -> Printf.printf "%d " x) l; print_endline "";;

print_string "DFS from 1: "; print_list (dfs g 1);;
print_string "DFS from 2: "; print_list (dfs g 2);;
print_string "DFS from 3: "; print_list (dfs g 3);;

print_string "BFS from 1: "; print_list (bfs g 1);;
print_string "BFS from 2: "; print_list (bfs g 2);;
print_string "BFS from 3: "; print_list (bfs g 3);;