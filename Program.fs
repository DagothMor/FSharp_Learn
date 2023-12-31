﻿// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

type F = 
      | AM
      | PM

type TimeOfDay = { hours: int; minutes: int; f: F }

type 'a cell = Nil | Cons of 'a * Lazy<'a cell>

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
        match xs1,xs2 with
        | [], [] -> []
        | [], hd2::tl2 -> plus(insert(xs1,hd2),tl2)
        | _, [] -> xs1
        //| hd1::tl1,_ when List.contains hd1 xs2 -> hd1 :: plus (insert(xs1,hd1),Remove (xs2,hd1))
        | _,hd2::tl2 -> plus(insert(xs1,hd2),tl2)

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


    // 41.4.1. Напишите функцию list_filter, которая реализует стандартную функцию List.filter, с помощью List.foldBack.

    let list_filter f xs =
        List.foldBack (fun x acc -> if f x then x::acc else acc) xs []

    // 41.4.2. Напишите функцию sum(p, xs), где p -- предикат int -> bool, и xs -- список целых.
    //Функция возвращает сумму тех элементов xs, для которых предикат истинен.
    //Реализуйте sum с помощью List.fold или List.foldBack.
    let sum (p, xs) =
        List.foldBack (+) (List.foldBack (fun x acc -> if p x then x::acc else acc) xs []) 0
         


    // 41.4.3. Напишите функцию revrev, которая получает на вход список списков, и перевёртывает как порядок вложенных списков, так и порядок элементов внутри каждого вложенного списка.
    
    //revrev [[1;2];[3;4;5]] = [[5;4;3];[2;1]]
    // Реализуйте revrev с помощью List.fold или List.foldBack.
    let revrev lst =
        List.fold (fun head tail -> (tail |> List.rev) ::head ) [] lst


    // Напишите функцию allSubsets, получающую целочисленные параметры n и k, и выдающую множество всех подмножеств множества
    // {1, 2, ..., n}, в которых ровно k элементов (0 <= k <= n).
    // 42.3
    let rec allSubsets n k =
        if n < 0 || k < 0 || k > n then
            Set.empty
        elif k = 0 then
            Set.singleton Set.empty
        else
            let subsetsWithoutN = allSubsets (n - 1) k
            let subsetsWithN = allSubsets (n - 1) (k - 1)
            Set.union subsetsWithoutN (Set.map (Set.add n) subsetsWithN)

    // 43.3
    let rec start (list, key) = 
        match list with 
            | (k, v) :: tail when k = key -> Some(v) 
            | (k, v) :: tail -> start (tail, key) 
            | [] -> None
    let try_find key m = 
        let list = Map.toList m
        start (list, key)

    // 47.4.1
    let f n =
        let mutable result = 1
        for i = 2 to n do
            result <- result * i
        result
    
    // 47.4.2
    let fibo n =
        let mutable a = 0
        let mutable b = 1
    
        for i = 0 to n-1 do
            let temp = a + b
            a <- b
            b <- temp
        a

    // 48.4.1. Напишите две версии функции вычисления n-го числа Фибоначчи Fn.
    // 1. fibo1: int -> int -> int -> int, с двумя параметрами-аккумуляторами n1 и n2, где
    // fibo1 n n1 n2 = Fn, n1 = второй начальный элемент последовательности, и n2 -- самый первый начальный.

    let factorial n =
        let rec f x a =
            if x <= 1 then a
            else f (x - 1) (a * x)
        f n 1

    // 48.4.1
    let rec fibo1 n n1 n2 = 
        if n = 1 then n1
        else fibo1 (n-1) (n1+n2) (n1) 
        
    // 2. fibo2: int -> (int -> int) -> int , где первый параметр -- n, а последний параметр -- функция-продолжение.

    // 48.4.2
    //let rec fibo2 n c = 
    //    if n = 1 then c 1
    //    else fibo2 (n-1) (fun f -> c(n * f))
    
    let rec fibo2 n c =
        match n with
        | 0 -> c 0
        | 1 -> c 1
        | _ -> fibo2 (n-1) (fun fn1 -> fibo2 (n-2) (fun fn2 -> c (fn1 + fn2)))


    let bigList n k =
      let rec f n acc =
        if n = 0 then k (acc)
        else f (n-1) (1::acc)
      f n []

    // 49.5.1. Определите последовательность чётных положительных чисел.
    let even_seq = Seq.initInfinite (fun i -> i+i)

    // 49.5.2. Определите последовательность факториалов неотрицательных целых чисел 1,1,2,6,...,n!

    let rec factorial n = 
      if n = 0 then 
        1
      else 
        (factorial (n - 1)) * n

    let fac_seq1 = Seq.initInfinite (fun i -> factorial i)

    // 49.5.3. Определите последовательность 0, -1, 1, -2, 2, -3, 3, ...
    let rec sequence n =
        match n with
        | 0 -> 0
        | _ ->
            //let prevSeq = sequence (n - 1)
            if n % 2 = 0 then
                n/2
            else
                -(n+1)/2

    let seq_seq = Seq.initInfinite (fun i -> sequence i)

    let fac_seq =
        let rec f n acc = seq {
            if n <= 1 then
                yield 1
            else 
                yield acc
            yield! f (n+1) (acc*(n+1))}
        f 0 1
    
    // 49.5.3
    
    let seq_seq =
        let rec sequence n = seq {
            if n % 2 = 0 then
                yield (n / 2)
            else 
                yield (-(n+1)/2)
            yield! sequence (n+1)
            }
        sequence 0
        
    let rec nat (n:int) : 'a cell = Cons (n, lazy(nat(n+1)))
    
    let n0 = nat 0

    //_______________________________________
    
    let hd (s : 'a cell) : 'a =
      match s with
        Nil -> failwith "hd"
      | Cons (x, _) -> x
    
    let tl (s : 'a cell) : Lazy<'a cell> =
      match s with
        Nil -> failwith "tl"
      | Cons (_, g) -> g
    
    
    // 51.3
    let rec nth (s : 'a cell) (n : int) : 'a =
      if n = 0 then
        hd s
      else
        nth ((tl s).Force()) (n - 1)
    
    // например, получить 30000-й элемент:
    let keke= nth n0 30000
    let sqr_cache = Seq.take 10 fac_seq |> Seq.toList
    0 // return an integer exit code
    
