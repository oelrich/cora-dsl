
type 'a prefix_tree =
    Root of 'a prefix_tree list * 'a list
    | Tree of char * 'a prefix_tree list * 'a list

let empty = Root([],[])

let add_to_node item tree =
    match tree with
    |Root(trees, data) -> Root(trees, item :: data)
    |Tree(c,trees, data) -> Tree(c, trees, item :: data)

let rec add_branch c item tree =
    match tree with
    | Tree(d, trees, _) -> tree 
    | _ -> tree

let add c item tree =
    match c with
    | None -> add_to_node item tree
    | _ -> add_branch c item tree
