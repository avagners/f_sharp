// 42.3
let rec allSubsets (n: int) (k: int) = // int -> int -> int set set
    match n, k with
    | _, 0 -> set [Set.empty]
    | n, k -> if n = k then set [set [1 .. n]]
                       else Set.union
                                (allSubsets (n - 1) k)
                                (Set.map (Set.add n) (allSubsets (n - 1) (k - 1)))