open Corabase.Types
(*
    name
        recordInfo
            dataDivider -> system
            id -> type & id
*)

let rec list_contains_data_divider (sys:string) (cl : cora list) =
    match cl with
    | Group({name="dataDivider"; _},{children=[
            Atomic({name="linkedRecordType"; _},{value="system"});
            Atomic({name="linkedRecordId"; _},{value=system})]; _}) :: _rest
    | Group({name="dataDivider"; _},{children=[
            Atomic({name="linkedRecordId"; _},{value=system});
            Atomic({name="linkedRecordType"; _},{value="system"})]; _}) :: _rest ->
        (String.equal system sys)
    | _ :: rest -> list_contains_data_divider sys rest
    | [] -> false

let rec list_contains_atom n v = function
    | (Atomic({name=n'; _}, {value = v'}):cora):: _ ->
        n = n' && v = v'
    | _::rest ->
        list_contains_atom n v rest
    | [] ->
        false

let match_record_info (sys:string) (id: string) = function
    | (Group({name="recordInfo";_}, {children = kids; _}):cora) ->
        (list_contains_atom "id" id kids) && (list_contains_data_divider sys kids)
    | _ -> false

let rec find_record (s: string) (t:string) (n:string) = function
    | c::cl ->
        List.append
            (if match_record s t n c then [c] else [])
            (find_record s t n cl)
    | [] -> []
and match_record (sys:string) (ty:string) (id:string)  = function
    | (Group({name=nt; _},{children = kids; _}):cora) ->
        (String.equal nt ty) &&
        (List.exists (match_record_info sys id) kids)
    | _ -> false

let find ((s,t,n): string * string * string) coras =
    let hit = find_record s t n coras in
    match List.length hit with
    | 1 -> List.hd hit
    | 0 -> failwith (Printf.sprintf ("Could not find %s: %s") t n)
    | c -> failwith (Printf.sprintf ("Found alarmingly many (%d) of %s: %s") c t n)

let rec find_id : cora list -> string option = function
    | Atomic({name = "id"; _}, {value = id}) :: _rest -> Some(id)
    | _ :: rest -> find_id rest
    | [] -> None

let get_id : cora -> string * string = function
    | Atomic(_, _) -> failwith "Could not read ID from record."
    | Group({name=type_name; _}, {children = kids; _}) ->
        match find_id kids with
        | Some(id) -> type_name, id
        | None -> failwith "Could not read ID from record."


module Struple = struct
    type t = string * string * string
    let compare (s0, s0', s0'') (s1, s1', s1'') =
        match Pervasives.compare s0 s1 with
        | 0 -> (match Pervasives.compare s0' s1' with
                | 0 -> Pervasives.compare s0'' s1''
                | c -> c)
        | c -> c
    
    let t_of_sexp tuple =
        Core.Tuple2.t_of_sexp Core.String.t_of_sexp Core.String.t_of_sexp tuple
    let sexp_of_t tuple =
        Core.Tuple2.sexp_of_t Core.String.sexp_of_t Core.String.sexp_of_t tuple
end

module NamedEdge = struct
    type t = string * Struple.t * Struple.t
    let compare (l0, l1, l2) (r0, r1, r2) =
        match Pervasives.compare l0 r0 with
        | 0 -> (match Pervasives.compare l1 r1 with
               | 0 -> Pervasives.compare l2 r2
               | c -> c)
        | c -> c
end

type named_struple = string * Struple.t

module EdgeMap = Map.Make(Struple)

module NamedEdgeSet = Set.Make(NamedEdge)

module NodeSet = Set.Make(Struple)

let cora_root = "recordType", "recordType"

let rec get_records = function
    | (Group({name = "recordList"; _}, {children = kids; _}) : cora) :: tail ->
        List.append kids (get_records tail)
    | _ :: _ -> failwith "This should have been empty ..."
    | [] -> []

let rec get_atom n = function
    | (Atomic({name=n'; _}, {value = value}):cora):: rest ->
        if n = n' then
            Some(value)
        else
            get_atom n rest
    | _::rest ->
        get_atom n rest
    | [] -> None

let get_link : cora -> (string * (string * string)) option = function
    | Group({name=name; _},{children=[
            Atomic({name="linkedRecordType"; _},{value=target});
            Atomic({name="linkedRecordId"; _},{value=id})]; _})
    | Group({name=name; _},{children=[
            Atomic({name="linkedRecordId"; _},{value=id});
            Atomic({name="linkedRecordType"; _},{value=target})]; _}) ->
        Some(name, (target, id))
    |_ -> None

let rec get_system (c:cora) =
    match c with
    | Group({name="dataDivider"; _},{children=[
            Atomic({name="linkedRecordType"; _},{value="system"});
            Atomic({name="linkedRecordId"; _},{value=system})]; _})
    | Group({name="dataDivider"; _},{children=[
            Atomic({name="linkedRecordId"; _},{value=system});
            Atomic({name="linkedRecordType"; _},{value="system"})]; _}) ->
        Some(system)
    | Group(_,{children=kids; _}) ->
        let rec ff elms =
            (match elms with
            | [] -> None
            | e :: rest ->
                (match get_system e with
                | Some(system) -> Some(system)
                | None -> ff rest))
            in
        ff kids
    |_ -> None

let rec get_links (c : cora) =
    match c with
    | Atomic(_,_) -> []
    | Group(_,{children = kids; _}) ->
        (get_link c) :: List.concat(List.map get_links kids)

let rec clean_list_of_opt = function
    | Some(v) :: rest -> v :: clean_list_of_opt rest
    | None :: rest -> clean_list_of_opt rest
    | [] -> []

let get_linking (c:cora) : (string * Struple.t) list =
    match get_system c with
    | Some(system) ->
        List.map
            (fun (name, (target, id)) -> (name, (system, target, id)))
            (clean_list_of_opt (get_links c))
    | None -> failwith "system not present"

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
        List.fold_left (fun s (name, target) ->
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


let graph_info (_edges: NamedEdgeSet.t) = ()



(*
    finished
    visit
    all
*)

(*
type attribute =
  { key: string;
    value: string }
type name =
  { name: string;
    repeat_id: string option; }
type group =
  { attributes: attribute list option;
    children: cora list; }
and atomic =
  { value: string; }
and cora =
  | Group of name * group
  | Atomic of name * atomic
*)