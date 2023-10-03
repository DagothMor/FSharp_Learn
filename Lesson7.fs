module Lesson7
// 20.3.1
let vat (n) (x) =
    let percentage = float n / 100.0
    x + (x * percentage)

// 20.3.2
let unvat (n) (x) =
    let percentage = float n / 100.0
    x / (1.0 + percentage)

// 20.3.3
let rec min f =
    let rec check n =
        if f n = 0 then n
        elif f n > 0 then check (n - 1)
        else check (n + 1)
    check 1