open Corabase.Types
open CoraTypes

let rec list_contains_atom n v = function
    | (Atomic({name=n'; _}, {value = v'}):cora):: _ ->
        n = n' && v = v'
    | _::rest ->
        list_contains_atom n v rest
    | [] ->
        false

let match_record_info (id: string) = function
    | (Group({name="recordInfo";_}, {children = kids; _}):cora) ->
        list_contains_atom "id" id kids
    | _ -> false

let rec find_record (t:string) (n:string) = function
    | c::cl ->
        List.append
            (if match_record t n c then [c] else [])
            (find_record t n cl)
    | [] -> []
and match_record (t:string) (n:string)  = function
    | (Group({name=nt; _},
            {attributes=record_attributes; children = kids}):cora) ->
        List.exists (match_record_info n) kids
    | _ -> false

let find ((t,n): string * string) coras =
    let hit = find_record t n coras in
    match List.length hit with
    | 1 -> List.hd hit
    | 0 -> failwith (Printf.sprintf ("Could not find %s: %s") t n)
    | c -> failwith (Printf.sprintf ("Found alarmingly many (%d) of %s: %s") c t n)

let rec find_id : cora list -> string option = function
    | Atomic({name = "id"}, {value = id}) :: rest -> Some(id)
    | _ :: rest -> find_id rest
    | [] -> None

let get_id : cora -> string * string = function
    | Atomic(_, _) -> failwith "Could not read ID from record."
    | Group({name=type_name; _}, {children = kids; _}) ->
        match find_id kids with
        | Some(id) -> type_name, id
        | None -> failwith "Could not read ID from record."


module Struple = struct
    type t = string * string
    let compare (s0, s0') (s1, s1') =
        match Pervasives.compare s0 s1 with
        | 0 -> Pervasives.compare s0' s1'
        | c -> c
    
    let t_of_sexp tuple =
        Core.Tuple2.t_of_sexp Core.String.t_of_sexp Core.String.t_of_sexp tuple
    let sexp_of_t tuple =
        Core.Tuple2.sexp_of_t Core.String.sexp_of_t Core.String.sexp_of_t tuple
end

module EdgeMap = Map.Make(Struple)

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
    | [] ->
        None

(*

let rec get_link_targets (cl: cora list) =
    match cl with
    | [] -> []
    | Atomic(_,_) :: rest -> get_link_targets rest
    | Group({name = name},{children = kids}) :: rest ->

        match match_link c with
        | Some(n,t,i) -> (n,t,i) :: get_link_targets rest


let get_links ((t,i):string*string) world =
    let c = find (t,i) world in


    ["",""]

let rec get_graph
        ((finished: Struple.t EdgeMap.t),
        (fringe: NodeSet.t))
        (world: cora list) =
    match NodeSet.choose_opt fringe with
    | Some(node) -> get_graph (visit node finished fringe world) world
    | None -> finished
and visit node (finished: Struple.t EdgeMap.t)
        (fringe: NodeSet.t)
        (world: cora list) =
    let linked_nodes = get_links node world in
    let new_finished =
        List.fold_left
            (fun m target -> EdgeMap.add node target  m)
            finished
            linked_nodes
        in
    let new_fringe =
        List.fold_left (fun st el -> NodeSet.add el st) fringe linked_nodes
        in
    new_finished, new_fringe

*)

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