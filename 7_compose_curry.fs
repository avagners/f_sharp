// 20.3.1
let vat (n: int) (x: float) : float = // int -> float -> float
    if n <= 0 || n > 100 then x
    else x * (1.0 + float(n) / 100.0) 

// 20.3.2
let unvat (n: int) (x: float) : float = // int -> float -> float
    if n <= 0 || n > 100 then x
    else x / (1.0 + float(n) / 100.0)

// 20.3.3
let rec min f = // (int -> int) -> int
    let rec findMin (n: int) : int =
        if f n = 0 then n
        else findMin (n + 1)
    findMin 1