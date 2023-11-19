// 50.2.1
let fac_seq =
    let rec f n acc = seq {
        if n <= 1 then
            yield 1
        else 
            yield acc
        yield! f (n+1) (acc*(n+1))}
    f 0 1

// 50.2.2
let seq_seq =
    let rec sequence n = seq {
        if n % 2 = 0 then
            yield (n / 2)
        else 
            yield (-(n+1)/2)
        yield! sequence (n+1)
        }
    sequence 0

