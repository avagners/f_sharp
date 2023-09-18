// 23.4.1
let get_result (bronze_after_operation: int) = // int -> int * int * int
    let gold: int = bronze_after_operation / 240
    let silver: int = (bronze_after_operation % 240) / 12
    let bronze: int = bronze_after_operation % 12
    (gold, silver, bronze)

let (.+.) x y = // int * int * int -> int * int * int -> int * int * int
    let sum_bronze (gold: int, silver: int, bronze: int) = gold * 240 + silver * 12 + bronze
    let bronze_after_operation: int = sum_bronze x + sum_bronze y
    get_result bronze_after_operation

let (.-.) x y = // int * int * int -> int * int * int -> int * int * int
    let sum_bronze (gold: int, silver: int, bronze: int) = gold * 240 + silver * 12 + bronze
    let bronze_after_operation: int = sum_bronze x - sum_bronze y
    get_result bronze_after_operation

// 23.4.2
let (.+) (a: float, b: float) (c: float, d: float) = (a + c, b + d) // float * float -> float * float -> float * float
let (.-) (a: float, b: float) (c: float, d: float) = (a - c, b - d) // float * float -> float * float -> float * float
let (.*) (a: float, b: float) (c: float, d: float) = (a * c - b * d, b * c + a * d) // float * float -> float * float -> float * float
let (./) (a: float, b: float) (c: float, d: float) = (a, b) .* (c / (c * c + d * d) , -d / (c * c + d * d)) // float * float -> float * float -> float * float