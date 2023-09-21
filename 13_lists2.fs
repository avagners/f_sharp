// 39.1
let rec rmodd = function // 'a list -> 'a list
    | [] | [_] -> []
    | _ :: second :: tail -> second :: rmodd tail

// 39.2
let rec del_even = function // int list -> int list
    | [] -> []
    | head :: tail -> if head % 2 = 0 then del_even tail
                      else head :: del_even tail

// 39.3
let rec multiplicity (x: 'a) (xs: 'a list) : int = // 'a -> 'a list -> int
    match xs with
    | [] -> 0
    | head :: tail -> if head = x then (multiplicity x tail) + 1 
                      else multiplicity x tail

// 39.4
let rec split = function // 'a list -> 'a list * 'a list
    | [] -> ([], [])
    | [x: 'a] -> ([x], [])
    | [x: 'a; y: 'a] -> ([x], [y])
    | first :: second :: tail ->
        let (a: 'a list, b: 'a list) = split tail
        (first :: a, second :: b)

// 39.5
let rec zip (xs1: 'a list, xs2: 'a list) : ('a * 'a) list = // 'a list * 'a list -> ('a * 'a) list
    if List.length xs1 <> List.length xs2 
        then failwith "The lengths of lists are not equal."
    else 
        match (xs1, xs2) with 
        | ([], []) -> []
        | (h_1 :: t_1, h_2 :: t_2) -> (h_1, h_2) :: zip (t_1, t_2)