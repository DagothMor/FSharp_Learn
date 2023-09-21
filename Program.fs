// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom




[<EntryPoint>]
let main argv =
    // 7.1.1. Напишите рекурсивную функцию fibo: int -> int,
    // где fibo(n) вычисляет n-е число Фибоначчи (n >= 0, fibo(0)=0, fibo(1)=1, ...).
    // Последовательность Фибоначчи начинается с двух значений 0,1, а n-й элемент равен
    // сумме n-1 - го и n-2 - го элементов: 0,1,1,2,3,5,8,13,...
    let rec fibo = function
        | 0 -> 0
        | 1 -> 1
        | 2 -> 1
        | n -> fibo (n-1) + fibo (n-2) 

    // 7.1.2. Напишите рекурсивную функцию sum: int -> int, 
    // где sum(n) вычисляет сумму первых n чисел ряда 1 + 2 + 3 + ... + n-1 + n
    // Параметр n -- натуральное число.

    let rec sum = function 
        | 0 -> 0
        | n -> n + sum (n-1)

    let rec sum2 = function 
    | (m,0) -> m 
    | (m,n) -> m + n + sum2(m,n-1)
        


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
    printfn "%d" (sum2 (5,4))
    0 // return an integer exit code
