// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom




[<EntryPoint>]
let main argv =
    let g (n) = n+5; // -- int -> int

    let gg = fun n -> n+5
    let h(x,y: float) = System.Math.Sqrt(x+y);
    // 
    let message = h (3.0,6.0) // Call the function
    printfn "%f" message
    0 // return an integer exit code
