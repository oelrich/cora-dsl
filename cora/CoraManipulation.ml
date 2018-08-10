open Corabase.Types

let get_children = function
    | Group(_, {children = kids; _}) -> kids
    | _ -> []

let get_type_name = function
    | Atomic({name = type_name; _}, _)
    | Group({name = type_name; _}, _) -> type_name

let rec flat_find_all_of_type type_name = function
    | c :: rest ->
        if String.equal type_name (get_type_name c) then
            c :: flat_find_all_of_type type_name rest
        else
            flat_find_all_of_type type_name rest
    | [] -> []

let rec deep_find query c =
    query c || List.exists (deep_find query) (get_children c)

let is_group = function
    | Group(_, _) -> true
    | _ -> false

let get_value = function
    | Atomic(_, {value = value}) -> value
    | _ -> failwith "no value in this data"

let is_atomic_type type_name = function
    | Atomic({name = type_name'; _}, _)->
        String.equal type_name type_name'
    | _ -> false

let is_group_type type_name = function
    | Group({name = type_name'; _}, _) ->
        String.equal type_name type_name'
    | _ -> false

let has_value value = function
    | Atomic(_, {value = value'}) ->
        String.equal value value'
    | _ -> false

let is_atomic_type_with_value type_name value c =
    is_atomic_type type_name c && has_value value c

let contains_atomic_type type_name c =
    List.exists (is_atomic_type type_name) (get_children c)

let is_record_info (c: cora) =
    is_group_type "recordInfo" c

let has_record_info (c:cora) =
    List.exists is_record_info (get_children c)

let get_record_info (c:cora) =
    List.find is_record_info (get_children c)

let get_record_id (c: cora) =
    if has_record_info c then
        get_children c
        |> List.find is_record_info
        |> get_children
        |> List.find (is_atomic_type "id")
        |> get_value
    else
        "no record info available"

let is_data_divider = function
    | Group({name="dataDivider"; _}, _) -> true
    | _ -> false

let get_system_id = function
    | Group({name = "dataDivider"; _}, {children = kids; _}) ->
        let system = List.find (is_atomic_type "linkedRecordId") kids
                     |> get_value in 
        Some(system)
    | _ -> None

let get_system (c: cora) =
    if has_record_info c then
        get_record_info c
        |> get_children
        |> List.find is_data_divider
        |> get_system_id
        |> function
            | None -> "not found, despite having record info"
            | Some(s) -> s
    else
        "no record info"

let get_record_identifier (c:cora) =
    get_system c, get_type_name c, get_record_id c

let is_link = function
    | Group(_ , {children = kids; _}) ->
        let linkType =
                flat_find_all_of_type "linkedRecordType" kids
                |> List.length in
        let linkId = flat_find_all_of_type "linkedRecordId" kids
                |> List.length in
        (match linkType, linkId with
        | 1, 1 -> true
        | 0, 0 -> false
        | _ -> failwith "invalid link")
    |_ -> false

let get_link = function
    | Group({name = link_name; _} , {children = kids; _}) ->
        let link_type =
                flat_find_all_of_type "linkedRecordType" kids
                |> List.hd |> get_value in
        let link_id = flat_find_all_of_type "linkedRecordId" kids
                |> List.hd |> get_value in
        link_name, link_type, link_id
    |_ -> failwith "invalid link"

let rec get_links (c:cora) : (string*string*string) list =
    let name = get_type_name c in
    let links =
        get_children c
        |> List.filter is_link
        |> List.map get_link in
    let deeper_links =
        get_children c
        |> List.filter (fun e -> not (is_link e) && (is_group e))
        |> List.map get_links
        |> List.concat in
    let all_links = List.append links deeper_links in
    List.map
        (fun (ln, lt, li) ->
            (Printf.sprintf ("%s:%s") name ln, lt, li)) all_links

let get_record_identifier_and_links (c:cora) =
    get_record_identifier c, get_links c
    

(*
get_system:     system (DataDivider)
get_type_name:  type_name (Record name)
get_record_id:  record_id (id)
*)