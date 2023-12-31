type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) (x: TimeOfDay) (y: TimeOfDay) = // TimeOfDay -> TimeOfDay -> bool
    if x.f > y.f then true
    elif x.f < y.f then false
    else x > y