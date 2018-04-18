open Corabase.Types

exception Parse_failure of string

type cora_system =
    | Alvin
    | Diva
    | Cora
    | JsClient
    | SystemOne
    | System of string

type cora_annotation = (string * string) list

type cora_user = {system: string; id: string }

type cora_entry_id = string

type cora_record_type = string

type cora_write_stamp =
    [`Created|`Updated] * cora_user * Core.Time.t

type cora_item_description =
    cora_system * string

type cora_record_info_type =
    {type_name: string; type_id: string;}

type cora_record_info = {
    id: string;
    record_type: cora_record_info_type;
    system: cora_system;
    created: cora_write_stamp;
    updated: cora_write_stamp list;
    }

type cora_item_ =
    | Empty
    | Node of cora_item_description
    | Linked of cora_item_description * (cora_item_ ref)

type cora_item = cora_entry_id * cora_item_

type cora_construct =
    | Empty
    | Entries of cora_construct list
    | RecordInfo of cora_record_info

type mix_group =
    { attributes: attribute list option;
    children: mix_cora list; }
and mix_atomic =
    { value: string; }
and mix_cora =
    | Pure of cora_construct
    | Group of name * cora_construct * mix_group
    | Atomic of name * atomic

let rec load_mix_cora (c:cora) =
    match c with
    | Group(name, {children = children; attributes= attribs}) ->
        Group(name, Empty,
            {attributes = attribs;
             children = List.map load_mix_cora children})
    | Atomic(name, atomic) ->
        Atomic(name, atomic)

let get_editor (c:cora) =
    match c with
    | Group({name = "updatedBy"; repeat_id = None},
            {children = [
                Atomic({name = "linkedRecordId"; repeat_id = None},
                       {value = user});
                Atomic({name = "linkedRecordType"; repeat_id = None},
                       {value = user_type})]; attributes = None}) ->
        Some(`Updated, {system = user_type; id = user})
    | Group({name = "createdBy"; repeat_id = None},
            {children = [
                Atomic({name = "linkedRecordId"; repeat_id = None},
                       {value = user});
                Atomic({name = "linkedRecordType"; repeat_id = None},
                       {value = user_type})]; attributes = None}) ->
        Some(`Created, {system = user_type; id = user})
    |_-> None

let get_timestamp (c: cora) =
    match c with
    | Atomic({name = "tsUpdated"; repeat_id = None}, {value = time }) ->
        Some(`Updated, Core.Time.of_string_fix_proto `Utc time)
    | Atomic({name = "tsCreated"; repeat_id = None}, {value = time }) ->
        Some(`Created, Core.Time.of_string_fix_proto `Utc time)
    |_ -> None

let rec de_opt_list = function
    | [] -> []
    | Some(v)::rest -> v:: de_opt_list rest
    | None::rest -> de_opt_list rest


let extract_write_stamp
        (u: ([> `Updated | `Created ] * cora_user) option)
        (t: ([> `Updated | `Created ] * Core.Time.t) option)
        (cl: cora list) (rem: cora list) =
    let times = List.map get_timestamp cl |> de_opt_list in
    let users = List.map get_editor cl |> de_opt_list in
    if (List.length times) = (List.length users) then
        List.sort (fun (t, _) (t', _) ->
                    match t,t' with
                    | `Updated, ` Created -> -1
                    | `Created, `Updated -> 1
                    | _ -> 0) times
    else
        failwith "Timestamps do not match"

(*    match rem, u, t with
    |_, Some(w, user), Some(w', time) when w = w' ->
            Some((w, user, time): cora_write_stamp), (List.append cl rem)
    |c::r, None, None ->
    |_ -> None, (List.append cl rem)
*)

let rec extract_record_type_entries (cl: cora list) (ri: cora_record_info) =
    match cl with
    | [] -> ri
    | el::rest ->ri

let extract_record_type (c:cora) =
    match c with
    | Group({name = "recordType"; repeat_id = None},
            {attributes = None; children = kids}) ->

                None
    | _ -> None



let lift_record_type (cm: mix_cora) =
    match cm with
    | Group(name, cs, group) -> cm
    | _ -> cm

let lift (cm: mix_cora) =
    lift_record_type cm


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