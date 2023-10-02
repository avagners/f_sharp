// 16.1
let notDivisible (n, m) = m % n = 0 // int * int -> bool

// 16.2
let prime n = // int -> bool
    let maxDivider = int (sqrt (float (n)))
    let rec notDiv (n, m) =
        match m with
        | 0 | 1 -> true
        | _ -> (n % m <> 0) && notDiv (n, m - 1)
    notDiv (n, maxDivider)