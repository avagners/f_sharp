type 'a cell = Nil | Cons of 'a * Lazy<'a cell>

let hd (s: 'a cell) : 'a = // 'a cell -> 'a
    match s with
    Nil -> failwith "hd"
    | Cons (x, _) -> x

let tl (s: 'a cell) : Lazy<'a cell> = // 'a cell -> Lazy<'a cell>
    match s with
    Nil -> failwith "tl"
    | Cons (_, (g: Lazy<'a cell>)) -> g


// 51.3
let rec nth (s: 'a cell) (n: int) : 'a = // 'a cell -> int -> 'a
    match n with
    | n when n < 0 -> failwith "'n' must be greater than 0"
    | 0 -> hd s
    | _ -> nth ((tl s).Force()) (n - 1)