
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
    let result = f 1
    if result = 0 then
        1
    else
        min f