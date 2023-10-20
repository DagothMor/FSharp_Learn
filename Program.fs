// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

type F = 
      | AM
      | PM

type TimeOfDay = { hours: int; minutes: int; f: F }

//exception LengthOfTwoArraysAreNotEqual

[<EntryPoint>]
let main argv =
    
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
    let rec del_even (xs) =
        match xs with
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
    let rec zip (xs1) (xs2) =
        match xs1, xs2 with
        | [], [] -> []
        | [], _ -> raise LengthOfTwoArraysAreNotEqual
        | _, [] -> raise LengthOfTwoArraysAreNotEqual
        | x :: xs, y :: ys -> (x, y) :: (zip xs ys)

    let rec mul = function
        | [] -> 0
        | [x] -> x
        | head :: tail -> head * mul tail

    let ans = zip [31..60] [1..30]


    0 // return an integer exit code
    
