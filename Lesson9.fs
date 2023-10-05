// 23.4.1
let (.+.) x y = 
    let ax,bx,cx = x
    let ay,by,cy = y
    let z = (ax*20 + bx)*12 + cx + (ay*20 + by)*12 + cy
    let zb = (z / 12)%20
    let za = z / 240
    let zc = z%12
    (za,zb,zc)

let (.-.) x y = 
    let ax,bx,cx = x
    let ay,by,cy = y
    let z = (ax*20 + bx)*12 + cx - ((ay*20 + by)*12 + cy)
    let zb = (z / 12)%20
    let za = z / 240
    let zc = z % 12
    (za,zb,zc)

// 23.4.2
let (.+) x y = 
    let a,b = x
    let c,d = y
    a+c,b+d

let (.*) x y = 
    let a,b = x
    let c,d = y
    a*c-b*d,b*c+a*d

let (.-) x y = 
    let a,b = x
    let c,d = y
    a-c,b-d

let (./) x y = 
    let a,b = x
    let c,d = y
    (a, b) .* (c / (c * c + d * d) , -d / (c * c + d * d))