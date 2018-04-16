open Corabase.Types
open Corabase.Json
open Corabase.Compare

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

let load_json dir =
    get_jsons_from_dir dir
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

let rec count_attributes (c:cora) (attribute_counts: string_counter) =
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

let print_string_option opt = function
    | None -> print_string opt
    | Some(str) -> print_string str

let print_attributes = function
    | None -> ()
    | Some((attribs: Corabase.Types.attribute list)) ->
        print_string " [ ";
        List.iter
            (fun {key = k; value = v} ->
                print_string k;
                print_string ": ";
                print_string v;
                print_string "; ") attribs;
        print_string "]"

let print_cora_name (n: name) =
    print_string n.name;
    match n.repeat_id with
    | None -> ()
    | Some(str) ->
        print_string " (";
        print_string str;
        print_string ")"

let rec print_indent = function
    | 0 -> ()
    | n -> print_char ' '; print_indent (n-1)


let rec print_cora indent = function
    | Atomic(name, value) ->
        print_indent indent;
        print_string "- ";
        print_cora_name name;
        print_string " = ";
        print_string value.value;
        print_newline ()
    | Group(name, {attributes = attribs; children = kids}) ->
        print_indent indent;
        print_string "> ";
        print_cora_name name;
        print_attributes attribs;
        print_newline ();
        List.iter (fun c -> print_cora (indent + 2) c) kids;
        ()


type search_pattern = Str.regexp

type search =
    | AttributeKey of search_pattern
    | AttributeValue of search_pattern
    | Name of search_pattern
    | Value of search_pattern


let hit (sp:search_pattern) (s:string) =
    Str.string_match sp s 0

let rec find_name (sp: search_pattern) (c: cora) =
    match c with
    | Atomic(name, _) ->
        if hit sp name.name then [c] else []
    | Group(name, group) ->
        let hits = List.map (find_name sp) group.children |> List.concat in
        if hit sp name.name then c :: hits else hits

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



let rec find_attribute_key (sp: search_pattern) (c: cora) =
    match c with
    | Atomic(_,_) -> []
    | Group(_, {attributes = attributes; children = children}) ->
        let hits =
            List.map (find_attribute_key sp) children
            |> List.concat in
        match attributes with
        | Some(attributelist) ->
            if List.exists (fun {key = k; value = _} -> hit sp k) attributelist then
                c :: hits
            else
                hits
        | None -> hits
let rec find_attribute_value (sp: search_pattern) (c: cora) =
    match c with
    | Atomic(_,_) -> []
    | Group(_, {attributes = attributes; children = children}) ->
        let hits =
            List.map (find_attribute_value sp) children
            |> List.concat in
        match attributes with
        | Some(attributelist) ->
            if List.exists (fun {key = _; value = v} -> hit sp v) attributelist then
                c :: hits
            else
                hits
        | None -> hits

let rec find_value (sp: search_pattern) (c: cora) =
    match c with
    | Atomic(_, value) ->
        if hit sp value.value then [c] else []
    | Group(_, group) ->
        List.map (find_value sp) group.children
        |> List.concat

let find (s:search) (c:cora) =
    match s with
    | AttributeKey(sp) -> find_attribute_key sp c
    | AttributeValue(sp) -> find_attribute_value  sp c
    | Name(sp) -> find_name sp c
    | Value(sp) -> find_value sp c
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