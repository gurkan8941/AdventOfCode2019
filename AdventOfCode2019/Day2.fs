module Day2

open System
open System.Collections.Generic

let additionProcess (currentIndex:int) (inputInts:int[]) =
    let sum = inputInts.[inputInts.[currentIndex + 1]] + inputInts.[inputInts.[currentIndex + 2]]
    let toReplacePostion = inputInts.[currentIndex + 3]
    inputInts.[toReplacePostion] <- sum

let multiplyProcess (currentIndex:int) (inputInts:int[]) =
    let multiply = inputInts.[inputInts.[currentIndex + 1]] * inputInts.[inputInts.[currentIndex + 2]]
    let toReplacePostion = inputInts.[currentIndex + 3]
    inputInts.[toReplacePostion] <- multiply

let processOpcode (currentIndex:int) (inputInts:int[]) =
    let start = inputInts.[currentIndex]
    match start with
    | 1 -> 
        additionProcess currentIndex inputInts
        currentIndex + 4
    | 2 -> 
        multiplyProcess currentIndex inputInts
        currentIndex + 4
    | 99 ->
        currentIndex
    | _ -> failwith "Unable to process Opcode at position " + currentIndex

let processIntsRec (inputInts:int[]) = 
    let rec loop currentIndex (inputInts:int[]) =
        let nextIndex = processOpcode currentIndex inputInts
        if nextIndex <= currentIndex then inputInts
        else
            loop nextIndex inputInts
    loop 0 inputInts

let replaceNounAndVerb (inputInts:int[]) noun verb = 
    let copy = Array.copy inputInts
    copy.[1] <- noun
    copy.[2] <- verb
    copy

let findNounAndVerb (inputInts:int[]) matchOutput =
    let possible = [0..99]
    let dict = new Dictionary<int, Tuple<int, int>>();
    for noun in possible do
        for verb in possible do
            let replaced = replaceNounAndVerb inputInts noun verb
            let output = (processIntsRec replaced).[0]
            if not(dict.ContainsKey(output)) then
                dict.Add(output, (noun, verb))
    if dict.ContainsKey(matchOutput) then
        dict.[matchOutput]
    else
        failwith ("Cannot find noun and verb for output " + matchOutput.ToString())

let calculateDay2 =
    let inputLines = Utils.readInput "Day2"
    let inputInts = inputLines.[0].Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
    let noun = 12
    let verb = 2
    let replaced = replaceNounAndVerb inputInts noun verb
    Console.WriteLine("****DAY 2****")
    let result = processIntsRec replaced
    Console.WriteLine("Part A: " + result.[0].ToString())
    let (n, v) = findNounAndVerb inputInts 19690720
    Console.WriteLine("Part B: " + (100 * n + v).ToString())