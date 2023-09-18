type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) (x: TimeOfDay) (y: TimeOfDay) = // TimeOfDay -> TimeOfDay -> bool
    if x.f = "AM" && y.f = "PM" then false
    elif x.f = "PM" && y.f = "AM" then true
    else x > y