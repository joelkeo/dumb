open Printf

module NeighborSet = Set.Make (String)

type neighborst = NeighborSet.t

module Graph = Map.Make (String)

type grapht = neighborst Graph.t

module StringSet = Set.Make(String)

type livet = StringSet.t

let empty : grapht = Graph.empty

let add_node (g : grapht) (name : string) : grapht =
  if Graph.mem name g then g else Graph.add name NeighborSet.empty g
;;

let add_directed_edge (g : grapht) (n1 : string) (n2 : string) : grapht =
  let g' = add_node (add_node g n1) n2 in
  let curr_neighbors = Graph.find n1 g' in
  Graph.add n1 (NeighborSet.add n2 curr_neighbors) g'
;;

let add_edge (g : grapht) (n1 : string) (n2 : string) : grapht =
  if n1 = n2 then g else
  let g' = add_directed_edge g n1 n2 in
  add_directed_edge g' n2 n1
;;

let has_edge (g : grapht) (n1 : string) (n2 : string) : bool =
  if n1 = n2 then false else
  let neighbs = (Graph.find n1 g) in
  StringSet.mem n2 neighbs
;;

let get_neighbors (g : grapht) (name : string) : string list =
  if Graph.mem name g
  then NeighborSet.fold (fun n ns -> n :: ns) (Graph.find name g) []
  else []
;;

let get_vertices (g : grapht) : string list =
  let keys, _ = List.split (Graph.bindings g) in keys
;;

let string_of_graph (g: grapht): string =
  let string_of_neighbors (n: string): string =
    ExtString.String.join ", " (get_neighbors g n)
  in
  ExtString.String.join "\n" (List.map (fun k -> sprintf "%s: %s" k (string_of_neighbors k)) (get_vertices g))
;;

(* appends two unrelated graphs *)
let append_graphs (g1: grapht) (g2: grapht) : grapht = 
  let g1s_added = 
    (* adding g1, or g1 and g2 combined nodes *)
    (Graph.fold 
    (fun name neighbors acc -> if Graph.mem name g2 then 
        Graph.add name (NeighborSet.union neighbors (Graph.find name g2)) acc
    else (Graph.add name neighbors acc))
    g1
    Graph.empty
   ) in
   (* adding g2 exclusive nodes *)
  (Graph.fold 
    (fun name neighbors acc -> if Graph.mem name g1 then acc else (Graph.add name neighbors acc))
    g2
    g1s_added)

let make_connections (root : string) (connections : StringSet.t) (g : grapht) : grapht = 
  StringSet.fold
  (fun connection gacc -> add_edge gacc root connection)
  connections
  g;;

(* adds initial frees, each of which should be connected to all of the previous *)
let add_nodes (nodes : string list) (g : grapht) =
  let (res_graph, _) = 
  (List.fold_left
  (fun (gacc, prev) node -> let added_node = (add_node gacc node)
in let connected = (make_connections node prev added_node) in
  (connected, NeighborSet.add node prev))
  (g, NeighborSet.empty)
  nodes) in
  res_graph
;;


let remove_neighbor (node_name : string) (ns : NeighborSet.t) : NeighborSet.t = 
  StringSet.filter
    (fun str -> (str != node_name))
    ns;;

let remove_node (node_name : string) (g : grapht) : grapht = 
  Graph.fold
  (fun name neighbs gacc -> (if name = node_name then gacc else (Graph.add name (remove_neighbor node_name neighbs) gacc)))
  g
  Graph.empty;;

let remove_nodes (nodes : string list) (g : grapht) : grapht = 
  List.fold_left
  (fun gacc curr -> (remove_node curr gacc))
  g
  nodes

let highest_degree (g : grapht) : string =
  let (name, degree) = 
  (Graph.fold
  (fun name neighbs (smallest, degree) -> 
    let n = (List.length (NeighborSet.elements neighbs)) in
      if n >= degree then (name, n) else (smallest, degree))
  g
  ("", -1)) in
  if name = "" then failwith "Bogus smallest degree" else
    name

let create_worklist (g : grapht) : (string list) =
  let (gacc, wl) = (Graph.fold
  (fun name neighbs (gacc, wl) -> let sd = (highest_degree gacc) in ((remove_node sd gacc), sd::wl))
  g
  (g, [])) in wl;;

  


