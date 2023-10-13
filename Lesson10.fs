module Lesson10
type TimeOfDay = { hours: int; minutes: int; f: string }

let getminutes (x:TimeOfDay) = 
    if x.f = "PM"
    then x.hours*60 + 720 + x.minutes
    else x.hours*60 + x.minutes

let (.>.) x y = (getminutes x) > (getminutes y)