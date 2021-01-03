module Day1

open System

let processModule mass =
    let result = int (floor ((float mass) / (float 3)) - float 2)
    result

let processModuleRec mass =
    let rec loop mass acc =
        if mass <= 0 then acc
        else
            let fuel = processModule mass
            let toadd = if fuel < 0 then 0 else fuel
            let sum = toadd + acc
            loop fuel sum
    loop mass 0
    

let calculateDay1 =
    let inputLines = Utils.readInput "Day1" |> Array.map int
    let totalSum = inputLines |> Array.map processModule |> Array.sum
    Console.WriteLine("****DAY 1****")
    Console.WriteLine("Part A: " + totalSum.ToString())
    let totalSumOfSum = inputLines |> Array.map processModuleRec |> Array.sum
    Console.WriteLine("Part B: " + totalSumOfSum.ToString())