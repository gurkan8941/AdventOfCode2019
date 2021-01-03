module Day7

open System
open System.Collections.Generic

type Memory = { MemoryInput: int[]; PointerIndex: int; LatestOutput: int; }
type ProcessMode = 
    | Position of value:int
    | Immediate of value:int

type Opcode =
    | Addition of output:int * index:int
    | Multiplication of output:int * index:int
    | Terminate of output:int * index:int
    | Input of output:int * index:int
    | JumpTrue of output:int * index:int
    | JumpFalse of output:int * index:int
    | LessThan of output:int * index:int
    | Equal of output:int * index:int
    | Output of output:int * index:int
    member x.Out = 
        match x with
        | Addition (output, _) | Multiplication (output,_) 
        | Terminate(output, _) | Input (output,_) -> output
        | JumpTrue (output,_) -> output
        | JumpFalse(output,_) -> output
        | LessThan (output,_) -> output
        | Equal (output,_) -> output
        | Output (output,_) -> output
    member x.Index = 
        match x with
        | Addition (_, index) -> index
        | Multiplication (_,index) -> index
        | Terminate(_,index) -> index
        | Input (_,index) -> index
        | JumpTrue (_,index) -> index
        | JumpFalse(_,index) -> index
        | LessThan (_,index) -> index
        | Equal (_,index) -> index
        | Output (_,index) -> index

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

let processOpcode (currentIndex:int) (inputInts:int[]) input latestoutput =
    let start = inputInts.[currentIndex]
    let padded = start.ToString().PadLeft(5, '0')
    let opCodeSequence = padded.Substring(3)
    match opCodeSequence with
    | "01" -> 
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let third = parseProcessMode (padded.Substring(0,1)) inputInts.[currentIndex + 3]
        addition first second third inputInts
        Addition(0, currentIndex + 4)
    | "02" -> 
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let third = parseProcessMode (padded.Substring(0,1)) inputInts.[currentIndex + 3]
        multiply first second third inputInts
        Multiplication(0, currentIndex + 4)
    | "03" ->
        let inputPosition = inputInts.[currentIndex + 1]
        inputInts.[inputPosition] <- input
        Input(0, currentIndex + 2)
    | "04" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let output = getValue first inputInts
        Output(output, currentIndex + 2)
    | "05" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts
        if firstOutput <> 0 then
            let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
            let secondOutput = getValue second inputInts
            JumpTrue(0, secondOutput)
        else
            JumpTrue(0, currentIndex + 3)
    | "06" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts
        if firstOutput = 0 then
            let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
            let secondOutput = getValue second inputInts
            JumpFalse(0, secondOutput)
        else
            JumpFalse(0, currentIndex + 3)
    | "07" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let secondOutput = getValue second inputInts
        if firstOutput < secondOutput then
            inputInts.[inputInts.[currentIndex + 3]] <- 1
        else
            inputInts.[inputInts.[currentIndex + 3]] <- 0
        LessThan(0, currentIndex + 4)
    | "08" ->
        let first = parseProcessMode (padded.Substring(2,1)) inputInts.[currentIndex + 1]
        let firstOutput = getValue first inputInts
        let second = parseProcessMode (padded.Substring(1,1)) inputInts.[currentIndex + 2]
        let secondOutput = getValue second inputInts
        if firstOutput = secondOutput then
            inputInts.[inputInts.[currentIndex + 3]] <- 1
        else
            inputInts.[inputInts.[currentIndex + 3]] <- 0
        Equal(0, currentIndex + 4)
    | "99" ->
        Terminate(latestoutput, currentIndex)
    | _ -> failwith ("Unable to process Opcode at position " + currentIndex.ToString())


let processIntsUntilHalt (inputInts:int[]) inputPhase inputSignal = 
    let rec loop currentIndex (inputInts:int[]) latestoutput =
        let input = if currentIndex = 0 then inputPhase else inputSignal
        let latestOpcode = processOpcode currentIndex inputInts input latestoutput
        match latestOpcode with 
        | Terminate (output, _) -> output
        | ty ->
            loop ty.Index inputInts ty.Out
    loop 0 inputInts 0

let processInts memory usePhase inputPhase inputSignal = 
    let rec loop currentIndex (inputInts:int[]) latestoutput =
        let input = if usePhase && currentIndex = 0 then inputPhase else inputSignal
        let latestOpcode = processOpcode currentIndex inputInts input latestoutput
        match latestOpcode with 
        | Output (output, nextIndex) -> (output, nextIndex, false)
        | Terminate (output, nextIndex) -> (output, nextIndex, true)
        | ty ->
            loop ty.Index inputInts ty.Out
    loop memory.PointerIndex memory.MemoryInput memory.LatestOutput


let distrib e L =
    let rec aux pre post = 
        seq {
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t 
        }
    aux [] L

let rec perms = function 
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distrib h) (perms t)

let rec processSequence (inputInts:int[]) (phaseSetting:int list) inputSignal =
    let rec loop (inputInts:int[]) currentAmplifier phase input =
        if currentAmplifier < 4 then
            let copy = (Array.copy inputInts)
            let output = processIntsUntilHalt copy phase input
            loop copy (currentAmplifier+1) phaseSetting.[currentAmplifier+1] output
        else
            let copy = (Array.copy inputInts)
            let output = processIntsUntilHalt copy phase input
            output
    loop inputInts 0 phaseSetting.[0] inputSignal

let rec processSequenceLoop (inputInts:int[]) (phaseSetting:int list) inputSignal =
    let dictionaryAmplifier = new Dictionary<int, Memory>()
    for i in [0..4] do
        dictionaryAmplifier.Add(i, {MemoryInput=(Array.copy inputInts); PointerIndex=0; LatestOutput=0 })
    let rec loop currentAmplifierStep phase input =
        
        let usePhase = (currentAmplifierStep <= 4)
        let currentAmplifier = currentAmplifierStep % 5
        let memory = dictionaryAmplifier.[currentAmplifier]
        let (output, nextIndex, terminate) = processInts memory usePhase phase input

        let updatedMemory = { memory with PointerIndex = nextIndex; LatestOutput = output}
        dictionaryAmplifier.[currentAmplifier] <- updatedMemory

        let nextAmplifierStep = currentAmplifierStep+1
        let nextPhaseSetting = phaseSetting.[(nextAmplifierStep % 5)]
        if terminate && currentAmplifier = 4 then output
        else
            loop nextAmplifierStep nextPhaseSetting output
    loop 0 phaseSetting.[0] inputSignal


let calculateDay7 =
    let inputLines = Utils.readInput "Day7"
    let inputInts = inputLines.[0].Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
    Console.WriteLine("****DAY 7****")
    let phaseSettingsA = [0..4]
    let allPermPhaseSettingsA = perms phaseSettingsA
    let resultA = allPermPhaseSettingsA |> Seq.toList |> List.map (fun setting ->  
            let r = processSequence inputInts setting 0
            (r, setting)) |> List.maxBy (fun (e, _) -> e)
    Console.WriteLine("Part A: "+ resultA.ToString())

    let phaseSettingsB = [5..9]
    let allPermPhaseSettingsB = perms phaseSettingsB
    let resultB = allPermPhaseSettingsB |> Seq.toList |> List.map (fun setting ->  
            let r = processSequenceLoop inputInts setting 0
            (r, setting)) |> List.maxBy (fun (e, _) -> e)
    Console.WriteLine("Part B: "+ resultB.ToString())