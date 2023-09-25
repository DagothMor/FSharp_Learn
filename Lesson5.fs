// 16.1
let notDivisible (n,m) = 
    m % n = 0


let rec primeRec(num, div) =
    match num, div with
    | n,d when d = 1 -> true
    | n,d when n%d = 0 -> false
    | n,d -> primeRec(n,d - 1)
// 16.2
let prime(num) =
    let maxDiv = System.Math.Sqrt(float num);
    if num < 2 then
        false
    else
        primeRec(num, int maxDiv)