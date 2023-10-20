
exception LengthOfTwoArraysAreNotEqual


let rec getEvent (lst) =
    match lst with
    | [] -> []
    | [x] -> [x]
    | x :: _ :: xs -> x :: (getEvent xs)

// 39.1
let rec rmodd (lst) =
    match lst with
    | [] -> []
    | [_] -> []
    | _ :: x :: xs -> x :: (rmodd xs)

// 39.2
let rec del_even (lst) =
    match lst with
    | [] -> []
    | x :: xs -> 
        if x % 2 <> 0 then
            x :: (del_even xs)
        else
            del_even xs


// 39.3
let rec multiplicity x xs = 
    match xs with
    | [] -> 0
    | head :: tail ->
        if head = x then
            1 + (multiplicity x tail)
        else
            multiplicity x tail

// 39.4
let rec split (xs) =
    let odd = rmodd(xs);
    let event = getEvent(xs);
    event,odd

// 39.5
let rec zip (xs1,xs2) =
    match xs1, xs2 with
    | [], [] -> []
    | [], _ -> raise LengthOfTwoArraysAreNotEqual
    | _, [] -> raise LengthOfTwoArraysAreNotEqual
    | x :: xs, y :: ys -> (x, y) :: (zip (xs,ys))

