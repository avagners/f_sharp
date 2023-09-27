// 50.2.1
let rec factorial = function // int -> int
    | 0 -> 1
    | n -> n * factorial (n - 1)

let fac_seq: int seq = seq { // seq<int>
    let mutable i: int = 0
    while true do 
        yield factorial i
        i <- i + 1
    }

// 50.2.2
let seq_seq: int seq = seq { // seq<int>
        for (i: int) in Seq.initInfinite id do 
            if i = 0 then 
                yield 0
            else 
                yield! seq [-i; i]
    }