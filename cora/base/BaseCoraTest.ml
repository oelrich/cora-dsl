

let%test "compare =" =
    let base_file = "./test_data/test.json" in
    let base = CoraJson.cora_of_json base_file in
    let save_file = "./tmp.json" in
    let reload =
        CoraJson.cora_to_json base save_file;
        CoraJson.cora_of_json save_file in
    (compare base reload) = 0
