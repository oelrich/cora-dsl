open Corabase.Types
open Core

type cora_system = Alvin | Cora | JsClient | SystemOne | System of string

type cora_annotation = (string * string) list

type cora_entry_id = string option


type cora_user = string

type cora_timestamp =
    | Created of cora_entry_id * Core.Time.t
    | Updated of cora_entry_id * Core.Time.t

type cora_item_description =
    cora_system * string

type cora_item_ =
    | Empty
    | Node of cora_item_description
    | Linked of cora_item_description * (cora_item_ ref)

type cora_item = cora_entry_id * cora_item_


type lift_type =
    | Err of cora * string
    | Parse of cora * cora_item
    | Done of cora_item


let lift_cora_time_stamp = function
    | Atomic({name = "tsCreated"; repeat_id = ri}, {value}) ->
        Some(Created(ri, Time.of_string value))
    | Atomic({name = "tsUpdated"; repeat_id = ri}, {value}) ->
        Some(Updated(ri, Time.of_string value))
    |_ -> None

let%test "lift cora_time_stamp created" =
    let stamp = Atomic({name = "tsCreated"; repeat_id = None}, {value = "2017-11-01 13:37:00.0"}) in
    (lift_cora_time_stamp stamp) = Some(Created(None, Time.of_string "2017-11-01 13:37:00.0"))

let%test "lift cora_time_stamp updated" =
    let stamp = Atomic({name = "tsUpdated"; repeat_id = None}, {value = "2017-11-01 13:37:00.0"}) in
    (lift_cora_time_stamp stamp) = Some(Updated(None, Time.of_string "2017-11-01 13:37:00.0"))



let lift_cora (c: cora)  =
    Empty


let lift (cl: cora list) =
    []


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