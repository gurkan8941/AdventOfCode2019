module Day1

open System

let hasSum2 element list sum =
    list |> Array.exists (fun e -> element + e = sum)

let hasSum3 element list sum =
    list |> Array.exists (fun e -> hasSum2 (element + e) list sum)
    

let calculateDay1 =
    let inputLines = Utils.readInput "Day1" |> Array.map int
    let elementsA = inputLines |> Array.filter (fun element -> hasSum2 element inputLines 2020)
    let multiA = elementsA |> Array.fold (fun acc e -> e*acc) 1
    Console.WriteLine("****DAY 1****")
    Console.WriteLine("Part A: " + multiA.ToString())
    let elementsB = inputLines |> Array.filter (fun element -> hasSum3 element inputLines 2020)
    let multiB = elementsB |> Array.fold (fun acc e -> e*acc) 1
    Console.WriteLine("Part B: " + multiB.ToString())
