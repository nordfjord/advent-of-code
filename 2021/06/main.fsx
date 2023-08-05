let parseInput (s: string) =
    let arr = Array.create 9 0L
    for s in s.Split(",") do
        let i = int s
        arr[i] <- arr[i] + 1L
    arr

let simulateDay (arr: _ array) =
    let zeroes = arr[0]
    arr[0] <- arr[1]
    arr[1] <- arr[2]
    arr[2] <- arr[3]
    arr[3] <- arr[4]
    arr[4] <- arr[5]
    arr[5] <- arr[6]
    arr[6] <- arr[7] + zeroes
    arr[7] <- arr[8]
    arr[8] <- zeroes

    
let rec simulate days arr =
    for i in 1..days do simulateDay arr
    Array.sum arr 

let input = System.Console.ReadLine() |> parseInput

input |> simulate 80  |> printfn "Part1: %A"
input |> simulate 256 |> printfn "Part2: %A"

