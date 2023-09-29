// 7.1.1
let rec fibo = function // int -> int
    | 0  -> 0
    | 1  -> 1
    | n  -> fibo (n - 1) + fibo (n - 2)

// 7.1.2
let rec sum = function  // int -> int
    | 1  -> 1
    | n  -> n + sum (n - 1)

// 7.1.3
let rec sum2 = function // int * int -> int
    | (m, 0) -> m
    | (m, n) -> m + n + sum2 (m, n - 1)