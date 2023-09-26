// 48.4.1
let rec fibo1 (n: int) (n1: int) (n2: int) : int = // int -> int -> int -> int
    match n with
    | 0 -> n2
    | _ -> fibo1 (n - 1) (n2 + n1) n1

// 48.4.2
let rec fibo2 (n: int) c : int = // int -> (int -> int) -> int
    match n with
    | 0 | 1 -> c n
    | _ -> fibo2 (n - 1) (fun _c1 -> fibo2 (n - 2) (fun _c2 -> c (_c1 + _c2)))

// 48.4.3
let rec bigList (n: int) k : int list = // int -> ('a list -> int list) -> int list
    let acc: int list = k []
    match n with
    | 0 -> acc
    | _ -> bigList (n-1) (fun _ -> 1 :: acc)