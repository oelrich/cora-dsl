open Corabase.Types
open CoraTypes

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

let rec print_cora indent (c:cora) =
    match c with
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

let print_system_name (cn: cora_system) =
    match cn with
    | Alvin -> print_string "Alvin"
    | Diva -> print_string "Diva"
    | Cora -> print_string "Cora"
    | JsClient -> print_string "JsClient"
    | SystemOne -> print_string "SystemOne"
    | System(name) -> print_string name

let print_write_stamp ((act, user, time): cora_write_stamp) =
    match act with
    |`Updated -> print_string "updated: "
    |`Created -> print_string "created: ";
    print_string user.system;
    print_string ":";
    print_string user.id;
    print_string ": ";
    print_string (Core.Time.to_string time);
    ()

let print_record_info indent (c: cora_record_info) =
    print_string "record_info";
    print_indent indent;
    print_system_name c.system;
    print_string ":";
    print_string c.record_type.type_name;
    print_string ": ";
    print_string c.record_type.type_id;
    print_newline ();
    print_indent indent;
    print_string "created: ";
    print_write_stamp c.created;
    print_newline ();
    ()

let rec print_cora_construct indent (c: cora_construct) =
    match c with
    | Entries(items) ->
        List.iter (print_cora_construct indent) items
    | RecordInfo(ri) ->
        print_record_info indent ri    
    | Empty -> ()

let rec print_mix indent (cm: mix_cora) =
    match cm with
    | Pure(cs) ->
        print_cora_construct indent cs;
        print_newline ()
    | Group(name, cs, {attributes = attribs; children = kids}) ->
        print_indent indent;
        print_string "> ";
        print_cora_name name;
        print_attributes attribs;
        print_newline ();
        List.iter (fun c -> print_mix (indent + 2) c) kids;
        print_newline ()
    | Atomic(name, atomic) ->
        print_indent indent;
        print_string "- ";
        print_cora_name name;
        print_string " = ";
        print_string atomic.value;
        print_newline ()


module StringSet = Set.Make(String)

let rec get_names depth prefix (cl: cora list) (cs: StringSet.t) =
    match depth, cl with
    | 0, _ -> cs
    | _, el::rest ->
        let csu = get_cora_names depth prefix el cs in
        get_names depth prefix rest csu
    | _, [] -> cs
and get_cora_names depth prefix (c: cora) (cs: StringSet.t) =
    match c with
    | Group({name = n; _}, {children = kids; _}) ->
        let new_prefix = Printf.sprintf ("%s/%s") prefix n in
        let csu = StringSet.add new_prefix cs in
        get_names (depth - 1) new_prefix kids csu
    | Atomic({name = n; _}, _) ->
        let new_prefix = Printf.sprintf ("%s/%s") prefix n in
        StringSet.add new_prefix cs

let get_name = function
    | (Atomic({name = n; _}, _) : cora) -> n
    | (Group({name = n; _}, _) : cora) -> n

let get_kids = function
    | (Group(_, {children = kids; _}) : cora) -> kids
    | _ -> []

let rec cora_dive (path: string list) (cl: cora list) =
    match path with
    | [] -> cl
    | step::rest ->
        cora_dive rest (collect_kids cl step)
and collect_kids (cl: cora list) (step: string) =
    List.filter (fun c -> String.equal step (get_name c)) cl
    |> List.map get_kids
    |> List.concat

let records cora_names =
    StringSet.filter (fun el ->
        Str.string_match
            (Str.regexp ("^/recordList")) el 0) cora_names
        |> StringSet.iter (fun el -> print_string el; print_newline ());;


let rec same_name name (cl: cora list) =
    match cl with
    | [] -> true
    | Atomic({name = n; _}, _)::rest ->
        String.equal name n && same_name name rest
    | Group({name = n; _}, _)::rest ->
        String.equal name n && same_name name rest

let rec print_names prefix (cl: cora list) =
    match cl with
    | el::rest ->
        print_cora_names prefix el;
        print_newline();
        print_names prefix rest
    | [] -> ()
and print_cora_names prefix (c: cora) =
    match c with
    | Group({name = n; _}, {children = kids; _}) ->
        let new_prefix = Printf.sprintf ("%s/%s") prefix n in
        print_string new_prefix ;
        print_names new_prefix kids
    | Atomic({name = n; _}, _) ->
        print_string (Printf.sprintf ("%s/%s") prefix n)