// 17.1
let rec pow = function // string * int -> string
    | (s: string, 0: int) -> ""
    | (s: string, n: int) -> s + pow(s, n - 1)

// 17.2
let rec isIthChar = function // string * int * char -> bool
    | (s: string, n: int, c: char) when n >= String.length s || n < 0 -> false 
    | (s: string, n: int, c: char) -> s.[n] = c

// 17.3
let rec occFromIth (s: string, n: int, c: char) = match n with // string * int * char -> int
    | n when n >= String.length s || n < 0 -> 0
    | n when s.[n] = c -> occFromIth (s, n + 1, c) + 1
    | _ -> occFromIth (s, n + 1, c)