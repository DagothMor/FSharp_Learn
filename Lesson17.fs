// 43.3
let rec start (list, key) = 
    match list with 
        | (k, v) :: tail when k = key -> Some(v) 
        | (k, v) :: tail -> start (tail, key) 
        | [] -> None
let try_find key m = 
    let list = Map.toList m
    start (list, key)