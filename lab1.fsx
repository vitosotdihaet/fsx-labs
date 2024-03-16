//! taylor series: 2x^2/2! - 2^3x^4/4! + ... + (-1)^(n-1)*2^(2n-1)x^(2n)/(2n)!

let a = 0.0
let b = 1.0
let eps = 0.00001
let step = 0.1


let factorial n =
    let rec factorial' acc k =
        if k <= 1 then acc
        else factorial' (acc * k) (k - 1)
    factorial' 1 n

let naiveTaylorSeries x eps =
    let rec naiveTaylorSeries' n acc =
        let sign = if n % 2 = 0 then -1.0 else 1.0
        let two_n = 2 * n
        let denominator = float(factorial two_n)
        let new_acc = sign * (2.0 ** (float(two_n - 1))) * (x ** (float(two_n))) / denominator + acc
        if abs (new_acc - acc) <= eps then (acc, n)
        else naiveTaylorSeries' (n + 1) new_acc
    naiveTaylorSeries' 1 0.0

let smartTaylorSeries x eps =
    let rec smartTaylorSeries' n previous acc =
        let two_n = 2 * n
        let current = -1.0 * previous * 2.0 ** 2.0 * x ** 2.0 / ((float(two_n) - 1.0) * float(two_n))
        let new_acc = acc + current
        if abs (new_acc - acc) <= eps then (acc, n)
        else smartTaylorSeries' (n + 1) current new_acc

    let first = 2.0 * x ** 2.0 / float(factorial 2)
    smartTaylorSeries' 2 first first

let printTable a b eps =
    printfn " x |     f(x)     | Naive Taylor | term # | Smart Taylor | term # "
    printfn "===|==============|==============|========|==============|========"

    let rec print_row (x: float) =
        if x >= b then ()
        else
            let fx = sin x ** 2.0
            let (fnt, fnt_terms) = (naiveTaylorSeries x eps)
            let (fst, fst_terms) = (smartTaylorSeries x eps)
            printfn "%2.1f| %1.10f | %1.10f | %6d | %1.10f | %6d " x fx fnt fnt_terms fst fst_terms
            print_row (x + step)

    print_row a

    printfn "------------------------------------------------------------------"


printTable a b eps
