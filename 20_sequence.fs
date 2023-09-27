// 49.5.1
let even_seq: int seq = // seq<int>
    Seq.initInfinite ( function n -> (n + 1) * 2 )

// 49.5.2
let rec factorial = function // int -> int
    | 0 -> 1
    | n -> n * factorial (n - 1)

let fac_seq: int seq = Seq.initInfinite (factorial) // seq<int>

// 49.5.3
let seq_seq: int seq = // seq<int>
    
    let _f = function
        | n when n % 2 = 0 -> n / 2 
        | n -> -(n + 1) / 2
    
    Seq.initInfinite (_f)
