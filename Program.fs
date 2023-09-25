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
       
        


     // Call the function
    printfn "%b" (prime(808))
    0 // return an integer exit code
