module Day4

open System
open System.Collections.Generic

let hasAdjacentEqualNumbers number =
    let list = number.ToString()
    let mutable latest = -1
    let mutable hasAdjacent = false
    for c in list do
        if int(Char.GetNumericValue(c)) = latest then 
            hasAdjacent <- true
        latest <- int(Char.GetNumericValue(c))
    hasAdjacent

let hasIncreasingNumbers number =
    let list = number.ToString()
    let mutable latest = 0
    let mutable hasDecreasing = false
    for c in list do
        if int(Char.GetNumericValue(c)) < latest then 
            hasDecreasing <- true
        latest <- int(Char.GetNumericValue(c))
    not(hasDecreasing)

let hasIncreasingNumbersRec number =
    let rec loop current latest index number =
        match current >= latest with
        | true -> 
            if index < number.ToString().Length - 1 then
                let nextIndex = index + 1
                let next = int(Char.GetNumericValue(number.ToString().[nextIndex]))        
                loop next current nextIndex number
             else 
                true
        | false -> false
    let start = int(Char.GetNumericValue(number.ToString().[0]))
    loop start -1 0 number

let hasAdjacentEqualNumbersRec number =
    let rec loop current latest index number =
        match current = latest with
        | true -> true
        | false -> 
            if index < number.ToString().Length - 1 then
                let nextIndex = index + 1
                let next = int(Char.GetNumericValue(number.ToString().[nextIndex]))        
                loop next current nextIndex number
             else 
                false
    let start = int(Char.GetNumericValue(number.ToString().[0]))
    loop start -1 0 number

let isValid number =
    hasIncreasingNumbersRec number && hasAdjacentEqualNumbersRec number

type Accumulator = { PreviousValue: int; SameValueLength: int; SameValueLengths: Dictionary<int, int list>; CurrentIndex:int; Length:int }

let addOrAppendValue (dictionary:Dictionary<int, int list>) value key =
    if dictionary.ContainsKey(key) then
       dictionary.[key] <- value :: dictionary.[key]
    else
        dictionary.[key] <- [value]

let calculateElementAdjacentFrequency (state:Accumulator) (element:int) =
    if element = state.PreviousValue then 
        if state.CurrentIndex = state.Length - 1 then
            addOrAppendValue state.SameValueLengths (state.SameValueLength + 1) state.PreviousValue
        {state with SameValueLength = state.SameValueLength + 1; CurrentIndex = state.CurrentIndex + 1}
    elif state.SameValueLength > 1 then
        addOrAppendValue state.SameValueLengths state.SameValueLength state.PreviousValue
        {PreviousValue = element; SameValueLength = 1; SameValueLengths = state.SameValueLengths; CurrentIndex = state.CurrentIndex + 1; Length = state.Length}
    else
        {PreviousValue = element; SameValueLength = 1; SameValueLengths = state.SameValueLengths; CurrentIndex = state.CurrentIndex + 1; Length = state.Length}


let hasAdjacentEqualNumberPairs number =
    let n = number.ToString()
    let numbers = Seq.toList n |> List.map (fun i -> int(Char.GetNumericValue(i)))
    let initalAcc = { PreviousValue = -1; SameValueLength = 0; SameValueLengths = new Dictionary<int, int list>(); CurrentIndex=0; Length=numbers.Length }
    let result = numbers |> List.fold calculateElementAdjacentFrequency initalAcc
    let pairSequences = result.SameValueLengths.Values |> Seq.concat |> Seq.filter (fun e -> e = 2) |> Seq.toList
    //let largerSequences = result.SameValueLengths.Values |> Seq.concat |> Seq.filter (fun e -> e > 2) |> Seq.toList
    pairSequences.Length > 0

let calculateDay4 =
    let inputLowerRange = 138241
    let inputUpperRange = 674034
    let range = [inputLowerRange..inputUpperRange]
    Console.WriteLine("****DAY 4****")
    let validNumbers = range |> List.filter isValid
    Console.WriteLine("Part A: " + validNumbers.Length.ToString())
    let validNumbersPartB = validNumbers |> List.filter hasAdjacentEqualNumberPairs
    Console.WriteLine("Part B: " + validNumbersPartB.Length.ToString())