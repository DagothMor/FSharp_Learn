module Lesson18
// 47.4.1
let f n =
    let mutable result = 1
    for i = 2 to n do
        result <- result * i
    result

// 47.4.2
let fibo n =
    let mutable a = 0
    let mutable b = 1

    for i = 0 to n-1 do
        let temp = a + b
        a <- b
        b <- temp
    a