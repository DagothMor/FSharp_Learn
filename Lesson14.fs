
// 40.1
let rec sum (p, xs) = 
    match xs with
    | [] -> 0
    | x :: xs -> 
        if p x then
            x + (sum (p,xs))
        else
            0 + sum (p,xs)

// 40.2.1
let rec count (xs, n) = 
    match xs with
    | [] -> 0
    | head :: tail ->
        if head = n then
            1 + (count (tail, n))
        elif head < n then
            0 + (count (tail, n))
        else 0

// 40.2.2
let rec insert (xs,n:int) =
    match xs with
    | [] -> [n]
    | hd::tl when n > hd -> hd ::(insert (tl, n))
    | hd::tl when n < hd -> ([n] @ xs)
    | hd::tl -> hd :: (insert (tl, n))

// 40.2.3
let rec Remove(xs,n) =
    match xs with
    | [] -> []
    | hd::tl when n = hd -> tl
    | hd::tl -> hd :: (Remove (tl, n))

let rec intersect (xs1, xs2) =
    match xs1 with
    | [] -> []
    | hd1::tl1 when List.contains hd1 xs2 -> hd1 :: intersect (tl1,Remove (xs2,hd1))
    | hd1::tl1 -> intersect (tl1,xs2)

// 40.2.4
let rec plus (xs1, xs2) =
    match xs1 with
    | [] -> []
    | hd1::tl1 when List.contains hd1 xs2 -> hd1 :: plus (insert(xs1,hd1),Remove (xs2,hd1))
    | hd1::tl1 -> plus (tl1,xs2)

// 40.2.5
let rec minus (xs1, xs2) =
    match xs1 with
    | [] -> []
    | hd1::tl1 when List.contains hd1 xs2 -> minus (Remove (xs1,hd1),Remove (xs2,hd1))
    | hd1::tl1 -> hd1 :: minus (tl1,xs2)

// 40.3.1
let smallest (lst) =
    match lst with
    | [] -> None
    | x::xs ->
        let rec findSmallest (min) (rest) =
            match rest with
            | [] -> min
            | y::ys when y < min -> findSmallest y ys
            | _::ys -> findSmallest min ys
        Some (findSmallest x xs)

// 40.3.2
let rec delete (n:int, xs) = Remove (xs,n)

// 40.3.3
let rec sort lst =
    match lst with
    | [] -> []
    | _ ->
        match smallest lst with
        | None -> []
        | Some(minVal) ->
            let rest = Remove(lst, minVal)
            minVal :: sort rest

let rec revrev lst =
    match lst with
    | [] -> []
    | x::xs -> revrev xs @ [x |> List.rev]
