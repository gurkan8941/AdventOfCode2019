module Day2

open System

type rule = { Min:int; Max:int; Letter:string; Password:string }

let parseRule (rawstring:string) =
    let indexDash = rawstring.IndexOf('-')
    let indexSpace = rawstring.IndexOf(' ')
    let indexColon = rawstring.IndexOf(':')
    let min = Int32.Parse(rawstring.Substring(0, indexDash))
    let max = Int32.Parse(rawstring.Substring((indexDash+1), indexSpace-((indexDash+1))))
    let letter = rawstring.Substring(indexSpace, indexColon-indexSpace).Trim()
    let password = rawstring.Substring(indexColon+1).Trim()
    { Min=min; Max=max; Letter=letter; Password=password }

let isValidA rule =
    let occurrence = rule.Password |> Seq.toList |> List.fold (fun acc e -> if e.ToString() = rule.Letter then acc + 1 else acc) 0
    let isValidResult = (occurrence >= rule.Min && occurrence <= rule.Max)
    isValidResult

let isValidB rule =
    let minPositionLetter = rule.Password.[rule.Min-1].ToString()
    let maxPositionLetter =rule.Password.[rule.Max-1].ToString()
    let isValidResult = 
        (minPositionLetter.ToString() = rule.Letter.ToString() ||
         maxPositionLetter.ToString() = rule.Letter.ToString()) &&
         not(
            minPositionLetter.ToString() = rule.Letter.ToString() && 
            maxPositionLetter.ToString() = rule.Letter.ToString()
            )
    isValidResult

let calculateDay2 =
    let inputLines = Utils.readInput "Day2"
    let validPasswordsA = inputLines |> Array.map (fun element -> parseRule element) |> Array.filter isValidA;
    Console.WriteLine("****DAY 2****")
    Console.WriteLine("Part A: " + validPasswordsA.Length.ToString())
    let validPasswordsB = inputLines |> Array.map (fun element -> parseRule element) |> Array.filter isValidB;
    Console.WriteLine("Part B: " + validPasswordsB.Length.ToString())
