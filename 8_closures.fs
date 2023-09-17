let curry f = fun (x: int) -> fun (y: int) -> f (x, y) : int // (int * int -> int) -> int -> int -> int

let uncurry f = fun (x: int, y: int) -> f x y : int // (int -> int ->int) -> int * int -> int