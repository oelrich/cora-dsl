(** Custom Json attribute for Cora_t.data_group. *)
(** Store the attribute as a key-value tuple.*)
type attribute = (string * string)
(** Load with #install Attribute.format;;*)
let format (fm: Format.formatter) ((key,value):attribute) =
    Format.fprintf fm "{\"%s\" = \"%s\"}" key value

let write_attribute ob (key,value) = (
    Bi_outbuf.add_char ob '{';
    Yojson.Safe.write_string ob key;
    Bi_outbuf.add_char ob ':';
    Yojson.Safe.write_string ob value;
    Bi_outbuf.add_char ob '}';
)
let read_attribute = (
    fun p lb ->
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_lcurl p lb;
        let key = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let value = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_object_end lb;
            Yojson.Safe.read_space p lb;
            (* read key *)
            key := Yojson.Safe.read_ident p lb;
            Ag_oj_run.read_until_field_value p lb;
            (* read value*)
            value := Ag_oj_run.read_string p lb;
            bits0 := !bits0 lor 0x1;
            while true do
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_object_end lb;
                Yojson.Safe.read_space p lb;
                (* read key *)
                key := Yojson.Safe.read_ident p lb;
                Ag_oj_run.read_until_field_value p lb;
                (* read value*)
                value := Ag_oj_run.read_string p lb;
                bits0 := !bits0 lor 0x1;
            done;
            assert false;
    with Yojson.End_of_object -> ((!key, !value) : attribute)
)
    

(*
type attribute = (string * string)
val unwrap: string -> attribute
val wrap: attribute -> string
*)