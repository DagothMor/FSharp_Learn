module Lesson16
// 42.3
let rec allSubsets n k =
    if n < 0 || k < 0 || k > n then
        Set.empty
    elif k = 0 then
        Set.singleton Set.empty
    else
        let subsetsWithoutN = allSubsets (n - 1) k
        let subsetsWithN = allSubsets (n - 1) (k - 1)
        Set.union subsetsWithoutN (Set.map (Set.add n) subsetsWithN)