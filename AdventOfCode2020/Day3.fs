module Day3

open System

let parseToArray (inputLines:string[]) =
    let rows = inputLines.Length
    let columns = inputLines.[0].Length
    let arr = Array2D.init columns rows (fun _ _ -> 0L)
    for x in [0..columns-1] do
        for y in [0..rows-1] do
            arr.[x,y] <- if inputLines.[y].[x].ToString() = "." then 0L else 1L
    arr

let countTrees (arr:int64[,]) (startposition:int*int) right down =
    let xbound = arr.GetLength(0)
    let ybound = arr.GetLength(1)
    let rec loop currentposition (acc:int64) =
        let xpos = (fst currentposition) % xbound
        let sum = arr.[xpos, snd currentposition] + acc
        let nextposition = (fst currentposition + right, snd currentposition + down)
        if snd nextposition >= ybound then 
            sum
        else
            loop nextposition sum
    loop startposition 0L

let calculateDay3 =
    let inputLines = Utils.readInput "Day3"
    let arr = parseToArray inputLines
    let right = 3
    let down = 1
    let startposition = (0,0)
    let trees = countTrees arr startposition right down   
    Console.WriteLine("****DAY 3****")
    Console.WriteLine("Part A: " + trees.ToString())
    let allTreesMulti = [(1,1);(3,1);(5,1);(7,1);(1,2)] 
                            |> List.map (fun (right, down) -> countTrees arr startposition right down)
                            |> List.fold (fun (acc:int64) (e:int64) -> acc*e) 1L
    Console.WriteLine("Part B: " + allTreesMulti.ToString())
