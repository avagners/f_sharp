// 49.5.1
let sqr: int seq = Seq.initInfinite ( // seq<int>
    function (i: int) -> (i + 1) * 2
) 

// 49.5.2
let rec factorial = function // int -> int
    | 0 -> 1
    | (n: int) -> n * factorial (n - 1)

let fac_seq: int seq = Seq.initInfinite factorial // seq<int>

// 49.5.3
let seq_seq: int seq = Seq.initInfinite ( // seq<int>
    function (i: int) -> if i % 2 = 0 then i / 2 
                         else -(i + 1) / 2
)
