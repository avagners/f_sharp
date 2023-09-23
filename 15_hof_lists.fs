// 41.4.1
let list_filter f (xs: 'a list) : 'a list = // ('a -> bool) -> 'a list -> 'a list
    let _filter (item: 'a) (acc: 'a list)  = if f item then item :: acc 
                                             else acc
    List.foldBack _filter xs []

// 41.4.2
let sum (p, xs: int list) = // (int -> bool) * int list -> int
    let _sum (acc: int) (item: int) : int = if p item then item + acc 
                                            else acc
    List.fold _sum 0 xs

// 41.4.3
let revrev = // int list list -> int list list
    let _func (acc: 'a list list) (item: 'a list) : 'a list list = (List.rev item) :: acc
    List.fold _func []