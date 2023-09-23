// 40.1
let rec sum (p, xs: int list) : int = // (int -> bool) * int list -> int
    match xs with
    | [] -> 0
    | head :: tail -> if p head 
                      then head + sum (p, tail)
                      else sum (p, tail)

// 40.2.1
let rec count (xs: int list, n: int) : int = // int list * int -> int
    match xs with
    | [] -> 0
    | head :: tail -> if head > n then 0
                      elif head = n then count (tail, n) + 1
                      else count (tail, n)

// 40.2.2
let rec insert (xs: int list, n: int) : int list = // int list * int -> int list
    match xs with
    | [] -> [n]
    | head :: tail -> if n > head then head :: insert (tail, n)
                      else n :: xs

// 40.2.3
let rec intersect (xs1: int list, xs2: int list) : int list = // int list * int list -> int list
    match xs1, xs2 with
    | [], _ | _, [] -> []
    | h1 :: t1, h2 :: t2 -> if h1 = h2 then h1 :: intersect (t1, t2)
                            elif h1 > h2 then intersect (xs1, t2)
                            else intersect (t1, xs2)

// 40.2.4
let rec plus (xs1: int list, xs2: int list) : int list = // int list * int list -> int list
    match xs1, xs2 with
    | [], [] -> []
    | xs1, [] -> xs1
    | [], xs2 -> xs2
    | h1 :: t1, h2 :: t2 -> if h1 = h2 then h1 :: h2 :: plus (t1, t2)
                            elif h1 > h2 then h2 :: plus (xs1, t2)
                            else h1 :: plus (t1, xs2)

// 40.2.5
let rec minus (xs1: int list, xs2: int list) : int list = // int list * int list -> int list
    match xs1, xs2 with
    | [], [] | [], _ -> []
    | x , [] -> x
    | h1 :: t1, h2 :: t2 -> if h1 < h2 then h1 :: minus (t1, xs2)
                            elif h1 > h2 then minus (xs1, t2)
                            else minus (t1, t2)

// 40.3.1
let rec smallest = function // int list -> int option
    | [] -> None
    | [x: int] -> Some x
    | first :: second :: tail -> if first <= second 
                                then smallest (first :: tail)
                                else smallest (second :: tail)

// 40.3.2
let rec delete (n: int, xs: int list) = // int * int list -> int list
    match xs with
    | [] -> []
    | head :: tail -> if head = n then tail
                      else head :: delete (n, tail)

// 40.3.3
let rec sort (xs: int list) : int list = // int list -> int list
    let smallestItem: int option = smallest xs
    match smallestItem with
    | None -> []
    | Some smallestItem -> smallestItem :: sort (delete (smallestItem, xs))

// 40.4
let rec revrev = function // 'a list list -> 'a list list
    | [] -> []
    | [x: 'a list] -> [List.rev x]
    | head :: tail -> revrev tail @ [List.rev head]