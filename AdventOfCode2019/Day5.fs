module Day5

open System
open System.Collections.Generic

type ProcessMode = 
    | Position of value:int
    | Immediate of value:int

type Opcode =
    | Addition of first:ProcessMode * second:ProcessMode * third:ProcessMode
    | Multiplication of first:ProcessMode * second:ProcessMode * third:ProcessMode
    | Terminate
    | Input of first:ProcessMode
    | Output of first:ProcessMode

let parseProcessMode input value =
    match input with
    | "0" -> Position(value)
    | "1" -> Immediate(value)
    | _ -> failwith ("Unable to parse ProcessMode for input " + input)

let getValue processMode (inputInts:int[]) =
    let value = 
        match processMode with
        | Position(v) -> inputInts.[v]
        | Immediate(v) -> v
    value

let addition first second third (inputInts:int[]) =
    let left = getValue first inputInts
    let right = getValue second inputInts
    let sum = left + right
    let toReplacePostion = 
        match third with
        | Position(v) -> v
        | _ -> failwith "Third parameter cannot be immediate mode"  
    inputInts.[toReplacePostion] <- sum

let multiply first second third (inputInts:int[]) =
    let left = getValue first inputInts
    let right = getValue second inputInts
    let multi = left * right
    let toReplacePostion = 
        match third with
        | Position(v) -> v
        | _ -> failwith "Third parameter cannot be immediate mode"  
    inputInts.[toReplacePostion] <- multi

let processOpcode (currentIndex:int) (inputInts:int[]) input =
    let start = inputInts.[currentIndex]
    let padded = start.ToString().PadLeft(5, '0')
    let opCodeSequence = padded.Substring(3)
    match opCodeSequence with
    | "01" -> 
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let third = parseProcessMode (padded.Substring(0,1)) inputInts.[currentIndex + 3]
        addition first second third inputInts
        (0, currentIndex + 4)
    | "02" -> 
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let third = parseProcessMode (padded.Substring(0,1)) inputInts.[currentIndex + 3]
        multiply first second third inputInts
        (0, currentIndex + 4)
    | "03" ->
        let inputPosition = inputInts.[currentIndex + 1]
        inputInts.[inputPosition] <- input
        (0, currentIndex + 2)
    | "04" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let output = getValue first inputInts
        if output <> 0 && inputInts.[currentIndex + 2] <> 99 then
            failwith "Error output "
        else
            (output, currentIndex + 2)
    | "05" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts
        if firstOutput <> 0 then
            let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
            let secondOutput = getValue second inputInts
            (0, secondOutput)
        else
            (0, currentIndex + 3)
    | "06" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts
        if firstOutput = 0 then
            let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
            let secondOutput = getValue second inputInts
            (0, secondOutput)
        else
            (0, currentIndex + 3)
    | "07" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let secondOutput = getValue second inputInts
        if firstOutput < secondOutput then
            inputInts.[inputInts.[currentIndex + 3]] <- 1
        else
            inputInts.[inputInts.[currentIndex + 3]] <- 0
        (0, currentIndex + 4)
    | "08" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let secondOutput = getValue second inputInts
        if firstOutput = secondOutput then
            inputInts.[inputInts.[currentIndex + 3]] <- 1
        else
            inputInts.[inputInts.[currentIndex + 3]] <- 0
        (0, currentIndex + 4)
    | "99" ->
        (0, currentIndex)
    | _ -> failwith ("Unable to process Opcode at position " + currentIndex.ToString())


let processIntsRec (inputInts:int[]) input = 
    let rec loop currentIndex (inputInts:int[]) =
        let (output, nextIndex) = processOpcode currentIndex inputInts input
        if output <> 0 then output
        else
            loop nextIndex inputInts
    loop 0 inputInts

let calculateDay5 =
    let inputLines = Utils.readInput "Day5"
    let inputInts = inputLines.[0].Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
    Console.WriteLine("****DAY 5****")
    let inputA = 1
    let result = processIntsRec (Array.copy inputInts) inputA
    Console.WriteLine("Part A: " + result.ToString())
    //let (n, v) = findNounAndVerb inputInts 19690720
    let inputB = 5
    let resultB = processIntsRec (Array.copy inputInts) inputB
    Console.WriteLine("Part B: " + resultB.ToString())