// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

type F = 
      | AM
      | PM

type TimeOfDay = { hours: int; minutes: int; f: F }



[<EntryPoint>]
let main argv =
    

    let getminutes (x:TimeOfDay) = 
        if x.f = F.PM
        then x.hours*60 + 720 + x.minutes
        else x.hours*60 + x.minutes

    let (.>.) x y = (getminutes x) > (getminutes y)
    // Call the function
    let a = {hours= 10; minutes= 10; f= F.AM}
    let b = {hours= 10; minutes= 10; f= F.PM}
    let ans = a .>. b

    let rec factorial = function 
        | 0  -> Some(1)
        | n when n > 0 -> Some(n * Option.get(factorial(n - 1)))
        | _  -> None

    //34.1. Напишите функцию upto: int -> int list, которая работает так:
    //upto n = [1; 2; ...; n].
    let rec help1 x y =
        if x > 0 then
        y-x :: help1 (x-1) y
        else
        []

    let rec upto = function
        | 1 -> [1]
        | n -> upto(n - 1) @ [n]

    //let rec upto = function
    //| 0  -> Some(1)
    //| n when n > 0 -> Some(n * Option.get(upto(n - 1)))
    //| _  -> None
        
    //let rec divv x = if x = 0 then None else  Some(divv (x-1)) :: x
    //let upto = divv 4  // r = Some(2)

    //34.2. Напишите функцию dnto: int -> int list, которая работает так:
    //downto n = [n; n-1; n-2; ...; 1].
    let rec dnto = function
    | 1 -> [1]
    | n when n > 0 -> n :: dnto (n - 1)
    //34.3. Напишите функцию evenn: int -> int list, которая генерирует список из первых n неотрицательных чётных чисел.
    let rec evenn = function
    | 1 -> [0]
    | 2 -> evenn(1) @ [2]
    | 3 -> evenn(2) @ [4]
    | n when n < 1 -> []
    | n -> evenn(n - 1) @ [2 * (n - 1)]

    let listup = upto(10)
    let listup1 = help1(10)
    0 // return an integer exit code
    //
    //| n when s.[n] = c -> 1 + occFromIth((s.Substring(1)),(n+1),c)
    //| n -> 0 + occFromIth((s.Substring(1)),(n+1),c)
