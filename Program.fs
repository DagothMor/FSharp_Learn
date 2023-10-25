// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

type F = 
      | AM
      | PM

type TimeOfDay = { hours: int; minutes: int; f: F }

exception LengthOfTwoArraysAreNotEqual

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
    let rec zip (xs1,xs2) =
        match xs1, xs2 with
        | [], [] -> []
        | [], _ -> raise LengthOfTwoArraysAreNotEqual
        | _, [] -> raise LengthOfTwoArraysAreNotEqual
        | x :: xs, y :: ys -> (x, y) :: (zip (xs,ys))

    let rec mul = function
        | [] -> 0
        | [x] -> x
        | head :: tail -> head * mul tail

    let even a = a % 2 = 0
    let odd a = not(even a)
    // 40.1. Напишите функцию sum(p, xs), где p -- предикат int -> bool, и xs -- список целых. Функция возвращает сумму тех элементов xs, для которых предикат истинен.
    let rec sum (p, xs) = 
        match xs with
        | [] -> 0
        | x :: xs -> 
            if p x then
                x + (sum (p,xs))
            else
                0 + sum (p,xs)
        
    // 40.2. Список [x1; x2; ...; xn] называется слабо восходящим, если его элементы удовлетворяют требованию x1 <= x2 <= ... <= xn
    // Следующие функции работают со слабо восходящими списками. При их реализации обязательно учитывайте эту специфику, оптимизируя вычисления.

    // 40.2.1. Напишите функцию count: int list * int -> int, которая подсчитывает количество вхождений числа в список.
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

    // 40.2.2. Напишите функцию insert: int list * int -> int list, которая добавляет новый элемент в список.
    // 40.2.2
    //let rec insert (xs: list * int, n:int) = 
    //    match xs with
    //    | [] -> [n]
    //    | head :: tail ->
    //        if tail = [] then
    //            head @ [n]
    //        else insert (xs,n)

    let rec insert (xs,n:int) =
        match xs with
        | [] -> [n]
        | hd::tl when n > hd -> hd ::(insert (tl, n))
        | hd::tl when n < hd -> ([n] @ xs)
        | hd::tl -> hd :: (insert (tl, n))

    // 40.2.3. Напишите функцию intersect: int list * int list -> int list, которая находит общие элементы в обоих списках, включая повторяющиеся.
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

    // 40.2.4. Напишите функцию plus: int list * int list -> int list, которая формирует список, объединяющий все элементы входных списков, включая повторяющиеся.

    // 40.2.4
    let rec plus (xs1, xs2) =
        match xs1 with
        | [] -> []
        | hd1::tl1 when List.contains hd1 xs2 -> hd1 :: plus (insert(xs1,hd1),Remove (xs2,hd1))
        | hd1::tl1 -> plus (tl1,xs2)

    // 40.2.5. Напишите функцию minus: int list * int list -> int list, которая возвращает список, содержащий элементы первого списка за исключением элементов второго списка (элементы, одинаковые по значению, считаются разными).
    
    // 40.2.5
    let rec minus (xs1, xs2) =
        match xs1 with
        | [] -> []
        | hd1::tl1 when List.contains hd1 xs2 -> minus (Remove (xs1,hd1),Remove (xs2,hd1))
        | hd1::tl1 -> hd1 :: minus (tl1,xs2)
    //let ans = count([0;1;1;1;2;2;2;3;3;3;4;4;4;5;5;5;5;5;6;7;8;9;10],5)
    

    // 40.3.1. Напишите функцию smallest: int list -> int option, которая возвращает наименьший элемент непустого списка.

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

    // 40.3.3. Напишите функцию сортировки с использованием предыдущих функций, которая сортирует входной список так, что на выходе получается слабо восходящий список.
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

    let ans = revrev ([[1;2;3];[4;5;6];[7;8;9]])
    0 // return an integer exit code
    
