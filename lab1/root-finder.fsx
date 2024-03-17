let eps = 0.0001

let rec dichotomy f a b =
    let m = (a + b) / 2.
    let fa = f(a)
    let fm = f(m)

    if abs(f(m)) < eps then m
    else if fm * fa > 0.0 then dichotomy f m b
    else dichotomy f a m

let rec iterations phi x0 =
    if abs (x0 - (phi x0)) < eps then
        x0
    else
        let next = phi x0
        iterations phi next


let newthon f f' x0 =
    let phi x : float = x - f(x)/f'(x)
    iterations phi x0

let f1 x = 2.0 * x * sin x - cos x

let f2 x = System.Math.Exp(x) + (1.0 + System.Math.Exp(2.0 * x))**0.5 - 2.0

let f3 x = System.Math.Log(x) - x + 1.8


let f1' x = 2.0 * sin x + 2.0 * x * cos x + sin x

let f2' x = System.Math.Exp(x) + System.Math.Exp(2.0 * x)/(1.0 + System.Math.Exp(2.0 * x))**0.5

let f3' x = 1.0/x - 1.0


let phi1 x = x - (2.0 * x * sin x - cos x) / (2.0 * sin x + x * cos x)

let phi2 x = x - (System.Math.Exp(x) + (1.0 + System.Math.Exp(2.0 * x))**0.5 - 2.0) / (System.Math.Exp(x) + System.Math.Exp(2.0 * x) / 2.0)

let phi3 x = x - (System.Math.Log(x) - x + 1.8) / (1.0 / x - 1.0)


printfn "func |   dichotomy   |  iterations   |     Newton     "
printfn "====================================================="
printfn "f1   |  %3.10f |  %3.10f |  %3.10f " (dichotomy f1  0.4 1.0) (iterations phi1 0.6) (newthon f1 f1' 0.6)
printfn "f2   | %3.10f | %3.10f | %3.10f " (dichotomy f2 -1.0 0.0) (iterations phi2 -0.5) (newthon f2 f2' -0.5)
printfn "f3   |  %3.10f |  %3.10f |  %3.10f " (dichotomy f3  2.0 3.0) (iterations phi3 2.5) (newthon f3 f3' 2.5)