open CoraBaseType

let backup
        try_second
        first
        second
        lhs_first
        lhs_second
        rhs_first
        rhs_second =
    let first_val =
        first
            lhs_first
            rhs_first
        in 
    if try_second first_val then
        second
            lhs_second
            rhs_second
    else
        first_val

let quad_compare
        comp_first
        comp_second
        lhs_first
        lhs_second
        rhs_first
        rhs_second =
    backup
        (fun x -> x == 0)
        comp_first
        comp_second
        lhs_first
        lhs_second
        rhs_first
        rhs_second

let rec compare_list_rec cmp (l : 'a list) (r: 'a list) =
    match l,r with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | lh :: lrest, rh :: rrest ->
        quad_compare
            cmp (compare_list_rec cmp)
            lh lrest
            rh rrest

let compare_list cmp (l : 'a list) (r: 'a list) =
    compare_list_rec cmp (List.sort cmp l) (List.sort cmp r)

let compare_attribute (la: attribute) (ra: attribute) =
    quad_compare
        String.compare String.compare
        la.key la.value
        ra.key ra.value

let compare_attributes
        (laso: attribute list option)
        (raso: attribute list option) =
    match laso,raso with
    | None, None -> 0
    | _, None -> 1
    | None, _ -> -1
    | Some(las), Some(ras) ->
        compare_list compare_attribute las ras

let%test "compare_attributes none" =
    let laso = (Some([]) : attribute list option) in
    let raso = (Some([]): attribute list option) in
    (compare_attributes laso raso) == 0

let%test "compare_attributes some" =
    let laso = (Some([{key = "bob"; value = "burger"}]) : attribute list option) in
    let raso = (Some([{key = "bob"; value = "burger"}]): attribute list option) in
    (compare_attributes laso raso) == 0

let%test "compare_attributes some >" =
    let laso = (Some([{key = "bob"; value = "burger"}; {key = "bobs"; value = "burgers"}]) : attribute list option) in
    let raso = (Some([{key = "bob"; value = "burger"}]): attribute list option) in
    (compare_attributes laso raso) > 0

let%test "compare_attributes some <" =
    let laso = (Some([{key = "bob"; value = "burger"}]) : attribute list option) in
    let raso = (Some([{key = "bob"; value = "burger"}; {key = "bobs"; value = "burgers"}]): attribute list option) in
    (compare_attributes laso raso) < 0

let%test "compare_attributes some <" =
    let laso = (Some([{key = "arts"; value = "art"}]) : attribute list option) in
    let raso = (Some([{key = "bob"; value = "burger"}; {key = "bobs"; value = "burgers"}]): attribute list option) in
    (compare_attributes laso raso) < 0

let compare_name (ln: name) (rn: name) =
    if not ((String.compare ln.name rn.name) == 0) then
        String.compare ln.name rn.name
    else
        match ln.repeat_id, rn.repeat_id with
        | None, None -> 0
        | _ , None -> 1
        | None, _ -> -1
        | Some(lrid), Some(rrid) ->
            String.compare lrid rrid

let%expect_test "compare_name N <" =
    let ln = ({name = "a"; repeat_id =  None } : name) in
    let rn = ({name = "a"; repeat_id =  Some("1") } : name) in
    print_endline (Printf.sprintf ("%d") (compare_name ln rn));
  [%expect {| -1 |}]


let%test "compare_name N >" =
    let ln = ({name = "a"; repeat_id =  Some("1") } : name) in
    let rn = ({name = "a"; repeat_id =  None } : name) in
    (compare_name ln rn) > 0

let%test "compare_name N =" =
    let ln = ({name = "a"; repeat_id =  None } : name) in
    let rn = ({name = "a"; repeat_id =  None } : name) in
    (compare_name ln rn) == 0

let%test "compare_name" =
    let ln = ({name = "a"; repeat_id =  Some("1") } : name) in
    let rn = ({name = "a"; repeat_id =  Some("1") } : name) in
    (compare_name ln rn) == 0

let%test "compare_name <" =
    let ln = ({name = "a"; repeat_id =  Some("1") } : name) in
    let rn = ({name = "a"; repeat_id =  Some("2") } : name) in
    (compare_name ln rn) < 0

let compare_atomic (la: atomic) (ra: atomic) =
    (String.compare la.value ra.value)

let%test "compare_atomic" =
    let la = ({value = "a"} : atomic) in
    let ra = ({value = "a"} : atomic) in
    (compare_atomic la ra) = 0

let%test "compare_atomic <" =
    let la = ({value = "a"} : atomic) in
    let ra = ({value = "b"} : atomic) in
    (compare_atomic la ra) < 0

let%test "compare_atomic >" =
    let la = ({value = "b"} : atomic) in
    let ra = ({value = "a"} : atomic) in
    (compare_atomic la ra) > 0

let de_opt_list_opt = function
    | None -> []
    | Some(lst) -> lst

let compare_list_option
        (cmp: ('a -> 'a -> int))
        (lhso : 'a list option)
        (rhso : 'a list option) =
    let lhs = de_opt_list_opt lhso in
    let rhs = de_opt_list_opt rhso in
    compare_list cmp lhs rhs

let rec compare_cora
        (atomic_group: int)
        (c_name: (name -> name -> int))
        (c_atomic: (atomic -> atomic -> int))
        (c_attribute_list_opt: (attribute list option -> attribute list option -> int))
        (c_list: ((cora -> cora -> int) -> cora list -> cora list -> int))
        (lc:cora) (rc:cora) =
    let rec cora_comp =
        fun (lc:cora) (rc:cora) ->
            match lc,rc with
            | Atomic(ln, la), Atomic(rn, ra) ->
                quad_compare
                    c_name c_atomic
                    ln la rn ra
            | Atomic(_,_), Group(_, _) -> atomic_group
            | Group(_,_), Atomic(_, _) -> -atomic_group
            | Group(ln,lg), Group(rn,rg) ->
                quad_compare
                    c_name group_comp
                    ln lg rn rg
    and group_comp = fun (lg:group) (rg:group) ->
            quad_compare
                c_attribute_list_opt
                (c_list cora_comp)
                lg.attributes (List.sort compare lg.children)
                rg.attributes (List.sort compare rg.children)
        in
    cora_comp lc rc
and compare (lc: cora) (rc: cora) =
    compare_cora
        1
        compare_name
        compare_atomic
        (compare_list_option compare_attribute)
        compare_list
        lc rc


let equal (lc: cora) (rc: cora) =
    (compare lc rc) == 0

let equal_cora
        atomic_group
        c_name
        c_atomic
        c_attibutelistopt
        c_coralist
        (lc:cora) (rc:cora) =
    (compare_cora
        atomic_group
        c_name
        c_atomic
        c_attibutelistopt
        c_coralist
        lc rc) == 0
