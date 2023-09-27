// 17.1
let rec pow(s,n) = 
    match string s,n with
    | s,n when n < 1 -> ""
    | s,n -> s + pow(s,(n-1))

// 17.2
let rec isIthChar(s,n,c) = 
        match (n) with
        | n when n < 0 -> false
        | n when n > String.length(s) -> false
        | n when n = 0 -> s.[0] = c
        | n -> isIthChar((s.Substring(1)),(n-1),c)

// 17.3
let rec occFromIth(s,n,c) = 
    match (n) with
    | n when n < 0 -> 0
    | n when n >= String.length(s) -> 0
    | n -> if s.[n] = c 
            then 1 + occFromIth(s,(n+1),c) 
            else 0 + occFromIth(s,(n+1),c)