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
    0 // return an integer exit code
    //
    //| n when s.[n] = c -> 1 + occFromIth((s.Substring(1)),(n+1),c)
    //| n -> 0 + occFromIth((s.Substring(1)),(n+1),c)
