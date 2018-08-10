open Corabase.Types
open Corabase.Json

module StringMap = Map.Make(String)

type string_counter = Int64.t StringMap.t

let print_string_int64 (text: string) (value: int64) =
    print_string "\"";
    print_string text;
    print_string "\": ";
    print_string (Int64.to_string value);
    print_newline ()

let print_string_counter (sc: string_counter) =
    StringMap.iter print_string_int64 sc

let increase (key: string) (counter: string_counter) =
    if StringMap.mem key counter then
        let count : int64 = StringMap.find key counter in
        StringMap.add key (Int64.add count 1L) counter
    else
        StringMap.add key 1L counter

let get_jsons_from_dir dir =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter
        (fun s -> Str.string_match (Str.regexp "^.+\\.json$") s 0)
    |> List.map (fun s -> String.concat "/" [dir; s])

let rec load = function
    | [] -> []
    | file::rest ->
        cora_of_json file :: load rest

let load_json dir rex =
    get_jsons_from_dir dir
    |> List.filter (fun s -> Str.string_match rex s 0)
    |> load

let rec summarise elements counter counts =
    match elements with
    | [] ->
        counts
    | element :: rest ->
        summarise rest counter (counter element counts)

let rec count_names (c:cora) (names: string_counter) =
    match c with
    | Atomic(name, _) ->
        increase name.name names
    | Group(name, group) ->
        let updated_counter = increase name.name names in
        List.fold_left (fun nc c -> count_names c nc) updated_counter group.children

let summarise_names coras =
    summarise coras count_names StringMap.empty

let count_attributes (c:cora) (attribute_counts: string_counter) =
    match c with
    | Atomic(_, _) ->
        attribute_counts
    | Group(_, group) ->
        let add_attributes nc a =
            increase a.key nc in
        let updated_counter =
            match group.attributes with
            |Some(attributes) ->
                List.fold_left
                add_attributes
                attribute_counts
                attributes
            | None -> attribute_counts in
        List.fold_left
            (fun nc c -> count_names c nc)
            updated_counter
            group.children

let summarise_attributes coras =
    summarise coras count_attributes StringMap.empty


type search_pattern = Str.regexp

type search =
    | AttributeKey of search_pattern
    | AttributeValue of search_pattern
    | Name of search_pattern
    | Value of search_pattern
    | Flat of search

let hit (sp:search_pattern) (s:string) =
    Str.string_match sp s 0

let get_name = function
    | Atomic(name, _) -> name
    | Group(name, _) -> name

let get_children = function
    | Atomic(_, _) -> []
    | Group(_, group) -> group.children

let rec find_rec (find_flat: search_pattern -> cora -> cora option) (sp: search_pattern) (c: cora) =
    let child_hits =
        List.map (find_rec find_flat sp) (get_children c)
        |> List.concat
        in
    match find_flat sp c with
    | Some(c) -> c :: child_hits
    | None -> child_hits

let find_name_flat (sp: search_pattern) (c: cora) =
    let name = get_name c in
    if hit sp name.name then
        Some(c)
    else
        None

let find_name (sp: search_pattern) (c: cora) =
    find_rec find_name_flat sp c


let%test "find_name" =
    let c = Group({name = "a"; repeat_id = None}, {attributes = None; children = []}) in
    find_name (Str.regexp "a") c
    |> List.length = 1

let%test "find_name 2" =
    let c = Group({name = "a"; repeat_id = None},
                  {attributes = None;children = [
                       Group({name = "a"; repeat_id = None}, {attributes = None; children = []})
                  ]}) in
    find_name (Str.regexp "a") c
    |> List.length = 2

let%test "find_name 3" =
    let c = Group({name = "a"; repeat_id = None},
                  {attributes = None;children = [
                       Group({name = "a"; repeat_id = None}, {attributes = None; children = []});
                       Atomic({name = "a"; repeat_id = None}, {value = "s"})
                  ]}) in
    find_name (Str.regexp "a") c
    |> List.length = 3

let%test "find_name 2 c" =
    let c = Group({name = "b"; repeat_id = None},
                  {attributes = None;children = [
                       Group({name = "a"; repeat_id = None}, {attributes = None; children = []});
                       Atomic({name = "a"; repeat_id = None}, {value = "s"})
                  ]}) in
    find_name (Str.regexp "a") c
    |> List.length = 2


let%test "find_name 1 c" =
    let c = Group({name = "aa"; repeat_id = None},
                  {attributes = None;children = [
                       Group({name = "aa"; repeat_id = None}, {attributes = None; children = []});
                       Atomic({name = "a"; repeat_id = None}, {value = "s"})
                  ]}) in
    find_name (Str.regexp "^a$") c
    |> List.length = 1

let get_attributes = function
    | Group(_, {attributes = Some(attribs); _ }) -> attribs
    | _ -> []

let find_attribute_key_flat (sp: search_pattern) (c: cora) =
    let attributes = get_attributes c in
    if List.exists (fun {key = k; value = _ } -> hit sp k) attributes then
        Some(c)
    else
        None

let find_attribute_key (sp: search_pattern) (c: cora) =
    find_rec find_attribute_key_flat sp c
 
let find_attribute_value_flat (sp: search_pattern) (c: cora) =
    match c with
    | Atomic(_,_) -> None
    | Group(_, {attributes = attributes; children = _children}) ->
        match attributes with
        | Some(attributelist) ->
            if List.exists (fun {key = _; value = v} -> hit sp v) attributelist then
                Some(c)
            else
                None
        | None -> None


let find_attribute_value (sp: search_pattern) (c: cora) =
    find_rec find_attribute_value_flat sp c

let find_value_flat (sp: search_pattern) (c: cora) =
    match c with
    | Atomic(_, value) ->
        if hit sp value.value then Some(c) else None
    | Group(_, _group) ->
        None

let rec find_value (sp: search_pattern) (c: cora) =
    match c with
    | Atomic(_, value) ->
        if hit sp value.value then [c] else []
    | Group(_, group) ->
        List.map (find_value sp) group.children
        |> List.concat

let rec find_flat (s:search) (c:cora) =
    match s with
    | AttributeKey(sp) ->
        find_attribute_key_flat sp c
    | AttributeValue(sp) ->
        find_attribute_value_flat  sp c
    | Name(sp) ->
        find_name_flat sp c
    | Value(sp) ->
        find_value_flat sp c
    | Flat(ss) -> find_flat ss c

let find (s:search) (c:cora) =
    match s with
    | AttributeKey(sp) ->
        find_attribute_key sp c
    | AttributeValue(sp) ->
        find_attribute_value  sp c
    | Name(sp) ->
        find_name sp c
    | Value(sp) ->
        find_value sp c
    | Flat(ss) ->
        match find_flat ss c with
        | Some(c) -> [c]
        | None -> []


let rec list_contains_atom n v = function
    | Atomic({name=n'; _}, {value = v'}):: _ ->
        n = n' && v = v'
    | _::rest ->
        list_contains_atom n v rest
    | [] ->
        false

let match_record_info (id: string) = function
    | Group({name="recordInfo";_}, {children = kids; _}) ->
        list_contains_atom "id" id kids
    | _ -> false

let rec find_record (t:string) (a:string*string) (n:string) = function
    | c::cl -> List.append
            (if match_record t a n c then [c] else [])
            (find_record t a n cl)
    | [] -> []
and match_record (_t:string) ((_k,_v):string*string) (n:string)  = function
    | Group({name=_t; _},
            {attributes=_record_attributes; children = kids}) ->
        List.exists (match_record_info n) kids
    | _ -> false


let rec flatten_cora = function
    | Group(_, {children = kids; _}) :: rest -> kids :: flatten_cora rest
    | _ :: rest -> flatten_cora rest
    | [] -> []


let load_flat_cora () =
    load_json "../data/files" (Str.regexp ".+")
    |> flatten_cora
    |> List.concat

(*
    | Parent of search

    | AtomicValue of search_pattern
    | NameWithRid of search_pattern * search_pattern
    | RidInGroup of search_pattern
    | LinkTarget of search_pattern * search_pattern


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