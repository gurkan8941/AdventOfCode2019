module Day3

open System

let processPosition (currentCoordinate:(int*int)) (position:string) =
    let start = position.[0]
    let digit = int(position.Substring(1))
    match start with
    | 'R' -> [1..digit] |> List.map (fun value -> (fst currentCoordinate + value, snd currentCoordinate))
    | 'L' -> [1..digit] |> List.map (fun value -> (fst currentCoordinate - value, snd currentCoordinate))
    | 'U' -> [1..digit] |> List.map (fun value -> (fst currentCoordinate, snd currentCoordinate - value))
    | 'D' -> [1..digit] |> List.map (fun value -> (fst currentCoordinate, snd currentCoordinate + value))
    | _ -> failwith ("Unable to parse position " + position)

let processWire wire =
    let rec loop currentCoordinate (wire:string[]) currentIndex aggregate =
        if wire.Length - 1 >= currentIndex then
            let position = wire.[currentIndex]
            let result = processPosition currentCoordinate position
            let aggregatePositions = List.append aggregate result
            let nextCoordinate = result.[result.Length - 1]
            let nextIndex = currentIndex + 1
            loop nextCoordinate wire nextIndex aggregatePositions
        else
           aggregate
    loop (0,0) wire 0 []

let manhattanDistance (coordinate1:int*int) (coordinate2:int*int) =
    let diffX = Math.Abs(fst coordinate1 - fst coordinate2)
    let diffY = Math.Abs(snd coordinate1 - snd coordinate2)
    diffX + diffY

let getStepsToCoordinate coordinate wireCoordinates =
    let rec loop coordinate wireCoordinates aggregate =
        match wireCoordinates with
        | head::tail -> 
            if head = coordinate then aggregate + 1
            else 
                let steps = aggregate + 1
                loop coordinate tail steps
        | [] -> failwith "Coordinate not found in wire"
    loop coordinate wireCoordinates 0

let calculateDay3 =
    let inputLines = Utils.readInput "Day3"
    let firstWire = inputLines.[0].Split(',', System.StringSplitOptions.RemoveEmptyEntries)
    let secondWire = inputLines.[1].Split(',', System.StringSplitOptions.RemoveEmptyEntries)
    Console.WriteLine("****DAY 3****")
    let firstWirePositions = processWire firstWire
    let secondWirePositions = processWire secondWire
    let intersect = Set.intersect (Set.ofList firstWirePositions) (Set.ofList secondWirePositions) |> Set.toList
    let minimalDistance = intersect |> List.map (fun v -> manhattanDistance v (0,0)) |> List.min
    Console.WriteLine("Part A: " + minimalDistance.ToString())
    let firstWireIntersectSteps = intersect |> List.map (fun item -> getStepsToCoordinate item firstWirePositions)
    let secondWireIntersectSteps = intersect |> List.map (fun item -> getStepsToCoordinate item secondWirePositions)
    let minimalSteps = List.zip firstWireIntersectSteps secondWireIntersectSteps |> List.map (fun i -> fst i + snd i) |> List.min
    Console.WriteLine("Part B: " + minimalSteps.ToString())