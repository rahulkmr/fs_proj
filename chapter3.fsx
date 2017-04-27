#load "chapter2.fsx"

let round x =
    if x >= 100 then 100
    elif x < 0 then 0
    else x


let round2 x =
    match x with
    | _ when x >= 100 -> 100
    | _ when x < 0 -> 0
    | _ -> x


let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)

let rec length l =
    match l with
    | [] -> 0
    | h :: t -> 1 + length t


let rec even n = (n = 0u) || odd(n - 1u)
and odd n = (n <> 0u) && even(n - 1u)


let oddPrimes = [3;5;7;11]
let morePrimes = [13;17]
let primes = 2 :: (oddPrimes @ morePrimes)
