open Cora_j
open Cora_t
open CoraBase

let check_cora_attributes = function
    | None -> None
    | Some(caul:Cora_t.attributes) ->
        Some(List.map
                (fun (key,value) -> {key = key; value = value})
                caul)

let rec cora_of_unchecked (cu:Cora_t.cora_unchecked) =
    let name = { name = cu.name; repeat_id = cu.repeat_id } in
    match cu.value, cu.attributes, cu.children with
    | Some(value), None, None ->
        Atomic((name, {value = value}))
    | None, _, _ ->
        Group((name, {
            attributes = check_cora_attributes cu.attributes;
            children =
                match cu.children with
                | None -> []
                | Some(children) ->
                    List.map cora_of_unchecked children
        }))
    | _ -> raise (Invalid_argument "Unchecked cora is atomic and group!")

let uncheck_cora_attributes = function
    | None -> None
    | Some(cal:attribute list) ->
        Some(List.map
                (fun {key = key; value = value} -> (key,value))
                cal)

let rec cora_to_unchecked = function
    | Atomic(name, value) ->
        ({name = name.name;
          repeat_id = name.repeat_id;
          value = Some(value.value);
          children = None;
          attributes = None } :Cora_t.cora_unchecked)
    | Group(name, group) ->
        let uc_attributes = uncheck_cora_attributes group.attributes in
        let uc_children =
                match group.children with
                | [] -> None
                | _ -> Some( List.map cora_to_unchecked group.children) in
        ({ name = name.name;
           repeat_id = name.repeat_id;
           value = None;
           children = uc_children;
           attributes = uc_attributes } : Cora_t.cora_unchecked)

let cora_of_json (file:string) =
    Ag_util.Json.from_file
        Cora_j.read_cora_unchecked
        file
    |> cora_of_unchecked

let cora_to_json (c: cora) (file:string) =
    Ag_util.Json.to_file
        Cora_j.write_cora_unchecked
        file
        (cora_to_unchecked c)

