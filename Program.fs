// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom




[<EntryPoint>]
let main argv =
    

    let notDivisible (n,m) = 
        m % n = 0

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
        
    let rec pow(s,n) = 
        match string s,n with
        | s,n when n < 1 -> ""
        | s,n -> s + pow(s,(n-1))
    

    let rec isIthChar(s,n,c) = 
            match (n) with
            | n when n < 0 -> false
            | n when n >= String.length(s) -> false
            | n when n = 0 -> s.[0] = c
            | n -> isIthChar((s.Substring(1)),(n-1),c)
    

    let rec occFromIth(s,n,c) = 
        match (n) with
        | n when n < 0 -> 0
        | n when n >= String.length(s) -> 0
        | n -> if s.[n] = c 
                then 1 + occFromIth(s,(n+1),c) 
                else 0 + occFromIth(s,(n+1),c)
    
    let vat1 n x = 
        if n < 0 || n > 100 then x
        else x + (x * (float n / 100.0))

    let unvat2 n x = 
        let y = vat1 n x
        let z = y
        y - ((y * float n) / 100.0)

    let vat (n) (x) =
        let percentage = float n / 100.0
        x + (x * percentage)

    let unvat (n) (x) =
        let percentage = float n / 100.0
        x / (1.0 + percentage)

    let f n = 
        n+5

    let rec min f =
        let rec check n =
            if f n = 0 then n
            elif f n > 0 then check (n - 1)
            else check (n + 1)
        check 1

    let myFunction x = x + 1
    // Call the function
    printfn "%d" (min (myFunction))
    0 // return an integer exit code
    //
    //| n when s.[n] = c -> 1 + occFromIth((s.Substring(1)),(n+1),c)
    //| n -> 0 + occFromIth((s.Substring(1)),(n+1),c)
