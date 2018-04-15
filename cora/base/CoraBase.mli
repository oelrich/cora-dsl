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
  