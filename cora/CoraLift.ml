open Corabase.Types
let rec list_contains_atom n v = function
    | (Atomic({name=n'; _}, {value = v'}):cora):: _ ->
        n = n' && v = v'
    | _::rest ->
        list_contains_atom n v rest
    | [] ->
        false

let match_record_info (id: string) = function
    | (Group({name="recordInfo"; _}, {children = kids; _}):cora) ->
        list_contains_atom "id" id kids
    | _ -> false

let rec find_record (t:string) (n:string) = function
    | c::cl -> List.append
            (if match_record t n c then [c] else [])
            (find_record t n cl)
    | [] -> []
and match_record (t:string) (n:string)  = function
    | (Group({name       = nt; _},
             {attributes = _record_attributes;
              children   = kids}):cora) when
                (String.equal t nt) ->
        List.exists (match_record_info n) kids
    | _ -> false



let cora_root = "metadata", "group", "metadataGroupGroup"


let rec get_records = function
    | (Group({name = "recordList"; _}, {children = kids; _}) : cora) :: tail ->
        List.append kids (get_records tail)
    | _ :: _ -> failwith "This should have been empty ..."
    | [] -> []

(*
    Cora
        A record is defined by its <type>Group




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