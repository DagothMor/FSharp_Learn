module Lesson20
// 48.4.1
let rec fibo1 n n1 n2 = 
    if n = 1 then n1
    else fibo1 (n-1) (n1+n2) (n1) 

// 48.4.2
let rec fibo2 n c =
    match n with
    | 0 -> c 0
    | 1 -> c 1
    | _ -> fibo2 (n-1) (fun fn1 -> fibo2 (n-2) (fun fn2 -> c (fn1 + fn2)))

// 48.4.3
let bigList n k =
  let rec f n acc =
    if n = 0 then k (acc)
    else f (n-1) (1::acc)
  f n []