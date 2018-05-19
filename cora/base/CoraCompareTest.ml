open CoraBaseType
open CoraCompare

let%test "compare atomic rid <" =
    let lc = (Atomic({name = "a"; repeat_id = Some("1")}, {value = "v"}) : cora) in
    let rc = (Atomic({name = "a"; repeat_id = Some("2")}, {value = "v"}) : cora) in
    (compare lc rc) < 0

let%test "compare atomic <" =
    let lc = (Atomic({name = "a"; repeat_id = None}, {value = "v"}) : cora) in
    let rc = (Atomic({name = "b"; repeat_id = None}, {value = "v"}) : cora) in
    (compare lc rc) < 0

let%test "compare atomic >" =
    let lc = (Atomic({name = "b"; repeat_id = None}, {value = "v"}) : cora) in
    let rc = (Atomic({name = "a"; repeat_id = None}, {value = "v"}) : cora) in
    (compare lc rc) > 0

let%test "compare atomic v >" =
    let lc = (Atomic({name = "a"; repeat_id = None}, {value = "v"}) : cora) in
    let rc = (Atomic({name = "a"; repeat_id = None}, {value = "vv"}) : cora) in
    (compare lc rc) < 0

let%test "compare atomic v <" =
    let lc = (Atomic({name = "a"; repeat_id = None}, {value = "vv"}) : cora) in
    let rc = (Atomic({name = "a"; repeat_id = None}, {value = "v"}) : cora) in
    (compare lc rc) > 0

let%test "compare group =" =
    let lc = (Group({name = "a"; repeat_id = None}, {children = []; attributes = None}) : cora) in
    let rc = (Group({name = "a"; repeat_id = None}, {children = []; attributes = None}) : cora) in
    (compare lc rc) == 0

let%test "compare group >" =
    let lc = (Group({name = "a"; repeat_id = None}, {children = []; attributes = None}) : cora) in
    let rc = (Group({name = "b"; repeat_id = None}, {children = []; attributes = None}) : cora) in
    (compare lc rc) < 0

let%test "compare group <" =
    let lc = (Group({name = "b"; repeat_id = None}, {children = []; attributes = None}) : cora) in
    let rc = (Group({name = "a"; repeat_id = None}, {children = []; attributes = None}) : cora) in
    (compare lc rc) > 0

let%test "compare group c =" =
    let lc = (Group({name = "a"; repeat_id = None},
                    {children = [
                        Atomic({name = "a"; repeat_id = None}, {value = "v"})];
                     attributes = None}) : cora) in
    let rc = (Group({name = "a"; repeat_id = None},
                    {children = [
                        Atomic({name = "a"; repeat_id = None}, {value = "v"})];
                     attributes = None}) : cora) in
    (compare lc rc) == 0

let%test "compare group c rev =" =
    let lc = (Group({name = "a"; repeat_id = None},
                    {children = [
                        Atomic({name = "a"; repeat_id = None}, {value = "v"});
                        Atomic({name = "b"; repeat_id = None}, {value = "v"})];
                     attributes = None}) : cora) in
    let rc = (Group({name = "a"; repeat_id = None},
                    {children = [
                        Atomic({name = "b"; repeat_id = None}, {value = "v"});
                        Atomic({name = "a"; repeat_id = None}, {value = "v"})];
                     attributes = None}) : cora) in
    (compare lc rc) == 0

let%test "compare group a <" =
    let lc = (Group({name = "a"; repeat_id = None},
                    {children = [];
                     attributes =  Some([{key ="a"; value ="a"}])}) : cora) in
    let rc = (Group({name = "a"; repeat_id = None},
                    {children = [];
                     attributes = Some([{key ="a"; value ="b"}])}) : cora) in
    (compare lc rc) < 0

let%test "compare group a =" =
    let lc = (Group({name = "a"; repeat_id = None},
                    {children = [];
                     attributes =  Some([{key ="a"; value ="a"}])}) : cora) in
    let rc = (Group({name = "a"; repeat_id = None},
                    {children = [];
                     attributes =  Some([{key ="a"; value ="a"}])}) : cora) in
    (compare lc rc) == 0

let%test "compare group a rev =" =
    let lc = (Group({name = "a"; repeat_id = None},
                    {children = [];
                     attributes =  Some([{key ="a"; value ="a"};{key ="b"; value ="b"}])}) : cora) in
    let rc = (Group({name = "a"; repeat_id = None},
                    {children = [];
                     attributes =  Some([{key ="b"; value ="b"};{key ="a"; value ="a"}])}) : cora) in
    (compare lc rc) == 0
