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



let basico (c: cora) =
    print_string "names:"; print_newline ();
    print_string_counter (count_names c StringMap.empty);
    print_string "attributes:"; print_newline ();
    print_string_counter (count_attributes c StringMap.empty)

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

