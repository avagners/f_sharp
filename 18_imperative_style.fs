// 47.4.1
let mutable factorial: int = 1
let mutable counter: int = 1

let f (n: int) : int =
    while counter <= n do
        factorial <- factorial * counter
        counter <- counter + 1
    factorial


// 47.4.2
let mutable previous = 0
let mutable current = 1
let mutable counter = 1

let fibo (n: int) : int = // int -> int
    if n = 0 then 0 else
    while counter < n do
        let next: int = previous + current
        previous <- current
        current <- next
        counter <- counter + 1
    current