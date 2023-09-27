// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom




[<EntryPoint>]
let main argv =
    
    // 16.1. Напишите функцию-предикат notDivisible: int * int -> bool,
    // где notDivisible(n,m) возвращает true, если число n -- делитель числа m.

    let notDivisible (n,m) = 
        m % n = 0

    // 16.2. Напишите функцию-предикат prime: int -> bool,
    // где prime(n) возвращает true, если n -- простое число.
    let rec prime(num, div) =
        match num, div with
        | n,d when d = 1 -> true
        | n,d when n%d = 0 -> false
        | n,d -> prime(n,d - 1)

    let prime(num) =
        let maxDiv = Math.Sqrt(float num);
        if num < 2 then
            false
        else
            prime(num, int maxDiv)
    
    let circleLen = fun R -> 2.0 * 3.14 * R
        

    // 17.1 Напишите функцию pow: string * int -> string, где pow(s,n) выдаёт строку s, повторенную n раз.
    let rec pow(s,n) = 
        match string s,n with
        | s,n when n < 1 -> ""
        | s,n -> s + pow(s,(n-1))
    
    // 17.2 Напишите функцию-предикат isIthChar: string * int * char -> bool, где isIthChar(s,n,c) проверяет, равен ли n-й (начиная с нуля) символ строки s символу c.
    let rec isIthChar(s,n,c) = 
            match (n) with
            | n when n < 0 -> false
            | n when n >= String.length(s) -> false
            | n when n = 0 -> s.[0] = c
            | n -> isIthChar((s.Substring(1)),(n-1),c)
    
    // 17.3 Напишите функцию occFromIth: string * int * char -> int, где occFromIth(s,n,c) 
    // возвращает количество вхождений символа с в строке s, начиная с позиции n.
    let rec occFromIth(s,n,c) = 
        match (n) with
        | n when n < 0 -> 0
        | n when n >= String.length(s) -> 0
        | n -> if s.[n] = c 
                then 1 + occFromIth(s,(n+1),c) 
                else 0 + occFromIth(s,(n+1),c)

     // Call the function
    printfn "%s" (pow("asd",0))
    0 // return an integer exit code
    //
    //| n when s.[n] = c -> 1 + occFromIth((s.Substring(1)),(n+1),c)
    //| n -> 0 + occFromIth((s.Substring(1)),(n+1),c)
