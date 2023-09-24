// 43.3
let try_find (key: 'a) (m: Map<'a, 'a0>) : option<'a0> = // 'a -> Map<'a, 'a0> -> option<'a0>
    if Map.containsKey key m 
    then Some (Map.find key m)
    else None