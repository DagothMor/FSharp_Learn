
// 49.5.1
let even_seq = Seq.initInfinite (fun i -> (i+1)*2)

// 49.5.2
let rec factorial n = 
    if n = 0 then 
      1
    else 
      (factorial (n - 1)) * n

let fac_seq = Seq.initInfinite (fun i -> factorial i)

// 49.5.3
let rec sequence n =
    match n with
    | 0 -> 0
    | _ ->
        //let prevSeq = sequence (n - 1)
        if n % 2 = 0 then
            n/2
        else
            -(n+1)/2

let seq_seq = Seq.initInfinite (fun i -> sequence i)