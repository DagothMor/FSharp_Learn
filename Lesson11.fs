module Lesson11
type F = 
    | AM
    | PM

type TimeOfDay = { hours: int; minutes: int; f: F }

let getminutes (x:TimeOfDay) = 
    if x.f = F.PM
    then x.hours*60 + 720 + x.minutes
    else x.hours*60 + x.minutes

let (.>.) x y = (getminutes x) > (getminutes y)
