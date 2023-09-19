// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom




[<EntryPoint>]
let main argv =
    // Напишите функцию, использующую сопоставление с образцом, которая получает на вход номер месяца (1-январь .. 12-декабрь) и возвращает целое число -- количество дней в нём, или 0, если номер месяца некорректный.
    let days_in_month = function 
        | 1  -> 31
        | 2  -> 28
        | 3 -> 31
        | 4  -> 30
        | 5  -> 31
        | 6 -> 30
        | 7  -> 31
        | 8  -> 31
        | 9 -> 30
        | 10  -> 31
        | 11  -> 30
        | 12 -> 31
        | _ -> 0

     // Call the function
    printfn "%d" (days_in_month 111)
    0 // return an integer exit code
