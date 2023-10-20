﻿module Lesson12
// 34.1
let rec upto = function
| 1 -> [1]
| n -> upto(n - 1) @ [n]

// 34.2
let rec dnto = function
    | 1 -> [1]
    | n when n > 0 -> n :: dnto (n - 1)

// 34.3
let rec evenn = function
| 1 -> [0]
| 2 -> evenn(1) @ [2]
| 3 -> evenn(2) @ [4]
| n when n < 1 -> []
| n -> evenn(n - 1) @ [2 * (n - 1)]