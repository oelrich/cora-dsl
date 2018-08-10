(*
open Corabase.Types
open CoraManipulation

let get_nodes (node, links) =
    node :: List.map (fun (_, ))
    
*)




module Record = struct
    type t = string * string * string
    let compare (s0, s0', s0'') (s1, s1', s1'') =
        match Pervasives.compare s0 s1 with
        | 0 -> (match Pervasives.compare s0' s1' with
                | 0 -> Pervasives.compare s0'' s1''
                | c -> c)
        | c -> c
end

module NamedEdge = struct
    type t = string * Record.t * Record.t
    let compare (l0, l1, l2) (r0, r1, r2) =
        match Pervasives.compare l0 r0 with
        | 0 -> (match Pervasives.compare l1 r1 with
               | 0 -> Pervasives.compare l2 r2
               | c -> c)
        | c -> c
end

module EdgeMap = Map.Make(Record)

module NamedEdgeSet = Set.Make(Record)

module NodeSet = Set.Make(Record)

let cora_root = "recordType", "recordType"

let get_linking _node = []
let find _node _world = ()
(*
let rec get_graph
        ((edges: NamedEdgeSet.t),
        (finished: NodeSet.t),
        (fringe: NodeSet.t))
        (world: cora list) =
    match NodeSet.choose_opt fringe with
    | Some(node) -> get_graph (visit edges node finished fringe world) world
    | None -> edges
and visit
        edges
        node
        (finished: NodeSet.t)
        (fringe: NodeSet.t)
        (world: cora list) =
    let linked_nodes = get_linking (find node world) in
    let new_edges =
        List.fold_left (fun s (name:string, target:Record.t) ->
            NamedEdgeSet.add (name, node, target) s
            ) edges linked_nodes
        in
    let new_finished =
        NodeSet.add node finished
        in
    let new_fringe =
        List.fold_left
            (fun s (_, target) -> NodeSet.add target s)
            (NodeSet.remove node fringe)
            (List.filter (fun (_, e) -> not (NodeSet.mem e finished))
                         linked_nodes)
        in
    new_edges, new_finished, new_fringe

*)
let graph_info (_edges: NamedEdgeSet.t) = ()
