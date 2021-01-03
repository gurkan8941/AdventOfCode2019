module Day9

open System
open System.Collections.Generic

type ProcessMode = 
    | Position of value:int64
    | Immediate of value:int64
    | RelativePosition of value:int64

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
    | "2" -> RelativePosition(value)
    | _ -> failwith ("Unable to parse ProcessMode for input " + input)

let getValue processMode (inputInts:int64[]) currentRelativeBase =
    let value = 
        match processMode with
        | Position(v) -> inputInts.[int(v)]
        | Immediate(v) -> v
        | RelativePosition(v) ->
            let position = currentRelativeBase + v
            inputInts.[int(position)]
    value

let getPosition processMode currentRelativeBase =
    let value = 
        match processMode with
        | Position(v) -> int(v)
        | Immediate(v) -> int(v)
        | RelativePosition(v) ->
            let position = currentRelativeBase + v
            int(position)
    value

let addition first second third (inputInts:int64[]) currentRelativeBase =
    let left = getValue first inputInts currentRelativeBase
    let right = getValue second inputInts currentRelativeBase
    let sum = left + right
    let toReplacePosition = getPosition third currentRelativeBase
    inputInts.[toReplacePosition] <- sum

let multiply first second third (inputInts:int64[]) currentRelativeBase =
    let left = getValue first inputInts currentRelativeBase
    let right = getValue second inputInts currentRelativeBase
    let multi = left * right
    let toReplacePosition = getPosition third currentRelativeBase
    inputInts.[int(toReplacePosition)] <- multi

let processOpcode (currentIndex:int) (inputInts:int64[]) (input:int64) (currentRelativeBase:int64) =
    let start = inputInts.[currentIndex]
    let padded = start.ToString().PadLeft(5, '0')
    let opCodeSequence = padded.Substring(3)
    match opCodeSequence with
    | "01" -> 
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let third = parseProcessMode (padded.Substring(0,1)) inputInts.[currentIndex + 3]
        addition first second third inputInts currentRelativeBase
        (0L, currentIndex + 4, currentRelativeBase, false)
    | "02" -> 
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let third = parseProcessMode (padded.Substring(0,1)) inputInts.[currentIndex + 3]
        multiply first second third inputInts currentRelativeBase
        (0L, currentIndex + 4, currentRelativeBase, false)
    | "03" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let position = getPosition first currentRelativeBase
        inputInts.[int(position)] <- input
        (0L, currentIndex + 2, currentRelativeBase, false)
    | "04" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let output = getValue first inputInts currentRelativeBase
        (output, currentIndex + 2, currentRelativeBase, true)
    | "05" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts currentRelativeBase
        if firstOutput <> 0L then
            let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
            let secondOutput = getValue second inputInts currentRelativeBase
            (0L, int(secondOutput), currentRelativeBase, false)
        else
            (0L, currentIndex + 3, currentRelativeBase, false)
    | "06" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts currentRelativeBase
        if firstOutput = 0L then
            let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
            let secondOutput = getValue second inputInts currentRelativeBase
            (0L, int(secondOutput), currentRelativeBase, false)
        else
            (0L, currentIndex + 3, currentRelativeBase, false)
    | "07" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts currentRelativeBase
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let secondOutput = getValue second inputInts currentRelativeBase
        let third = parseProcessMode (padded.Substring(0,1)) inputInts.[currentIndex + 3]
        let position = getPosition third currentRelativeBase
        if firstOutput < secondOutput then
            inputInts.[int(position)] <- 1L
        else
            inputInts.[int(position)] <- 0L
        (0L, currentIndex + 4, currentRelativeBase, false)
    | "08" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts currentRelativeBase
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let secondOutput = getValue second inputInts currentRelativeBase
        let third = parseProcessMode (padded.Substring(0,1)) inputInts.[currentIndex + 3]
        let position = getPosition third currentRelativeBase
        if firstOutput = secondOutput then
            inputInts.[int(position)] <- 1L
        else
            inputInts.[int(position)] <- 0L
        (0L, currentIndex + 4, currentRelativeBase, false)
    | "09" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts currentRelativeBase
        (0L, currentIndex + 2, currentRelativeBase + firstOutput, false)
    | "99" ->
        (0L, currentIndex, currentRelativeBase, false)
    | _ -> failwith ("Unable to process Opcode at position " + currentIndex.ToString())


let processIntsRec (inputInts:int64[]) input currentRelativeBase = 
    let rec loop currentIndex (inputInts:int64[]) relativeBase outputs =
        let (output, nextIndex, relativeBase, outputCode) = processOpcode currentIndex inputInts input relativeBase
        if nextIndex = currentIndex then
            outputs |> List.rev
        else 
            if outputCode then
                let aggregate = output::outputs
                loop nextIndex inputInts relativeBase aggregate
            else
                loop nextIndex inputInts relativeBase outputs
    loop 0 inputInts currentRelativeBase []

let calculateDay9 =
    let inputLines = Utils.readInput "Day9"
    let inputInts = inputLines.[0].Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int64
    let moreMemorySlots = 50000
    let moreMemory = Array.init moreMemorySlots (fun _ -> 0L)
    let memory = Array.append inputInts moreMemory
    Console.WriteLine("****DAY 9****")
    let inputA = 1L
    let relativeBase = 0L
    let resultA = processIntsRec (Array.copy memory) inputA relativeBase
    Console.WriteLine("Part A: " + resultA.ToString())
    let inputB = 2L
    let resultB = processIntsRec (Array.copy memory) inputB relativeBase
    Console.WriteLine("Part B: " + resultB.ToString())