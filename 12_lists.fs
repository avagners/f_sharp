// 34.1
let rec upto (n: int) : int list = // int -> int list
    let rec _upto (m: int, acc: int list) : int list =
        if m = 1 then m :: acc
        else _upto (m - 1, m :: acc)
    if n < 1 then []
    else _upto (n, [])

// 34.2
let rec dnto = function // int -> int list
    | n when n < 1 -> []
    | 1 -> [1]
    | n -> n :: dnto (n - 1)

// 34.3
let rec evenn (n: int) : int list = 
    let rec _evenn (m: int, acc: int list) : int list =
        if m = 0 then m :: acc
        else _evenn (m - 2, m :: acc)
    if n < 1 then []
    else _evenn (n * 2 - 2, [])
