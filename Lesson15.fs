module Lesson15
// 41.4.1
let list_filter f xs = 
    List.foldBack (fun x acc -> if f x then x::acc else acc) xs []

// 41.4.2
let sum (p, xs) = 
    List.foldBack (+) (List.foldBack (fun x acc -> if p x then x::acc else acc) xs []) 0

// 41.4.3
let revrev lst =
    List.fold (fun head tail -> (tail |> List.rev) ::head ) [] lst