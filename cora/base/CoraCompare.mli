open CoraBase


val compare_list_option: ('a -> 'a -> int) -> 'a list option -> 'a list option -> int

val compare_attribute: attribute -> attribute -> int

val compare_name: name -> name -> int

val compare_atomic: atomic -> atomic -> int

val compare: cora -> cora -> int

val compare_cora:
        int ->
        (name -> name -> int) ->
        (atomic -> atomic -> int) ->
        (attribute list option -> attribute list option -> int) ->
        ((cora -> cora -> int) -> cora list -> cora list -> int) ->
        cora -> cora -> int

val equal: cora -> cora -> bool

val equal_cora:
        int ->
        (name -> name -> int) ->
        (atomic -> atomic -> int) ->
        (attribute list option -> attribute list option -> int) ->
        ((cora -> cora -> int) -> cora list -> cora list -> int) ->
        cora -> cora -> bool