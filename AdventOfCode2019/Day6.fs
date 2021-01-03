module Day6

open System
open System.Collections.Generic

let addIfNotExists value parent (dict:Dictionary<string, string>) =
    if not(dict.ContainsKey(value))
    then 
        dict.Add(value, parent) |> ignore

let parseLine (input:string) (aggregateNodes:Dictionary<string, string>) =
    let separatorIndex = input.IndexOf(')')
    let first = input.Substring(0, separatorIndex)
    let second = input.Substring(separatorIndex + 1)
    addIfNotExists second first aggregateNodes
        
let parseLines inputlines =
    let dict = new Dictionary<string, string>()
    for inputline in inputlines do
        parseLine inputline dict
    dict

let countOrbit key (dictionary:Dictionary<string, string>) =
    let rec loop key aggregate =
        if dictionary.ContainsKey(key) then
            let parent = dictionary.[key]
            loop parent (aggregate+1)
        else
            aggregate
    loop key 0

let getParents key (dictionary:Dictionary<string, string>) =
    let rec loop key aggregate =
        if dictionary.ContainsKey(key) then
            let parent = dictionary.[key]
            loop parent (key::aggregate)
        else
            aggregate
    loop key []

let run (dictionary:Dictionary<string, string>) =
    dictionary.Keys |> Seq.toList |> List.map (fun key -> countOrbit key dictionary) |> List.sum 

let calculateDay6 =
    let inputLines = Utils.readInput "Day6"
    let dictionary = parseLines inputLines
    Console.WriteLine("****DAY 6****")
    let result = run dictionary
    Console.WriteLine("Part A: " + result.ToString())
    let orbitalYOU = dictionary.["YOU"]
    let orbitalSAN = dictionary.["SAN"]
    let orbitalYOUparents = (getParents orbitalYOU dictionary) |> List.rev
    let orbitalSANparents = (getParents orbitalSAN dictionary) |> List.rev
    let intersect = Set.intersect (Set.ofList orbitalYOUparents) (Set.ofList orbitalSANparents) |> Set.toList
    let firstIntersectYOU = (orbitalYOUparents |> List.mapi (fun i e -> (e,i)) |> List.filter (fun (e,_) -> List.contains e intersect)
         |> List.sortBy (fun e -> snd e) |> List.take 1).[0]
    let firstIntersectSAN = (orbitalSANparents |> List.mapi (fun i e -> (e,i)) |> List.filter (fun (e,_) -> List.contains e intersect)
         |> List.sortBy (fun e -> snd e) |> List.take 1).[0]
    let resultB = (snd firstIntersectYOU) + (snd firstIntersectSAN)
    Console.WriteLine("Part B: " + resultB.ToString())