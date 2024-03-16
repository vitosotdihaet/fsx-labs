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

let naiveTaylorSeries x n =
    let rec term k =
        if k > n then 0.0
        else
            let sign = if k % 2 = 0 then -1.0 else 1.0
            let numerator = float (2 * k)
            let denominator = float (factorial (2 * k))
            (sign * (2.0 ** numerator) * (x ** numerator)) / denominator + term (k + 1)
    term 1

let smartTaylorSeries x eps =
    let rec term k acc =
        let sign = if k % 2 = 0 then -1.0 else 1.0
        let numerator = float (2 * k)
        let denominator = float (factorial (2 * k))
        let termValue = (sign * (2.0 ** numerator) * (x ** numerator)) / denominator
        if abs termValue < eps then acc
        else term (k + 1) (acc + termValue)
    term 1 0.0

let printTable f a b eps =
    printfn " x |   f(x)   | Naive Taylor | Smart Taylor"
    printfn "===|==========|==============|============="

    let rec print (x: float) =
        if x >= b then 0
        else
            let fx = f x
            printfn "%2.1f| %8.6f |" x fx
            print (x + step)
            // printfn "%2.1f| %8.6f | %11.6f | %10.6f" x fx naiveTerms smartTerms
    // let mutable xVal = a
    // while xVal <= b do
    //     let fx = f xVal
    //     let naiveTerms = naiveTaylorSeries xVal 10
    //     let smartTerms = smartTaylorSeries xVal eps
    //     printfn "%2.1f| %8.6f | %11.6f | %10.6f" xVal fx naiveTerms smartTerms
    //     xVal <- xVal + x
    print a
    printfn "--------------------------------------------"


printTable f a b eps
