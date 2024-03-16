//! taylor series: 2x^2/2! - 2^3x^4/4! + ... + (-1)^(n-1)*2^(2n-1)x^(2n)/(2n)!

let f x = sin x ** 2.0

let a = 0.0
let b = 1.0
let eps = 0.0001
let step = 0.1


let factorial n =
    let rec factorial' acc k =
        if k <= 1 then acc
        else factorial' (acc * k) (k - 1)
    factorial' 1 n

let naiveTaylorSeries x eps =
    let rec sum i acc =
        let sign = if i % 2 = 0 then -1.0 else 1.0
        let two_n = 2 * i
        let denominator = float (factorial two_n)
        let new_acc = sign * (2.0 ** (float(two_n - 1))) * (x ** (float two_n)) / denominator + acc
        if abs (new_acc - acc) <= eps then acc
        else sum (i + 1) new_acc
    sum 1 0.0

let smartTaylorSeries x eps =
    // let rec term k acc =
    //     let sign = if k % 2 = 0 then -1.0 else 1.0
    //     let numerator = float (2 * k)
    //     let denominator = float (factorial (2 * k))
    //     let termValue = (sign * (2.0 ** numerator) * (x ** numerator)) / denominator
    //     if abs termValue < eps then acc
    //     else term (k + 1) (acc + termValue)
    // term 1 0.0
    0.0

let printTable f a b eps =
    printfn " x |     f(x)     | Naive Taylor | Smart Taylor "
    printfn "===|==============|==============|=============="

    let rec print (x: float) =
        if x >= b then ()
        else
            let fx = f x
            printfn "%2.1f| %1.10f | %1.10f | %1.10f " x fx (naiveTaylorSeries x eps) (smartTaylorSeries x eps)
            print (x + step)

    print a

    printfn "------------------------------------------------"


printTable f a b eps
