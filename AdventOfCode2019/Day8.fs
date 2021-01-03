module Day8

open System
open System.Collections.Generic

type State = { Width:int; Height:int; CurrentIndex:int; ImageLayers: int[,] list; }

let getLayersCount currentIndex width height =
    let remainder = currentIndex % (width * height)
    let layersCount = (currentIndex - remainder) / (width * height)
    layersCount + 1

let getY currentIndex width height =
    let remainder = currentIndex % (width)
    let y = (currentIndex - remainder) / (width)
    let y' = y % height
    y'

let foldFunction state element =
    let layersCount = getLayersCount state.CurrentIndex state.Width state.Height
    let x = state.CurrentIndex % state.Width
    let y = getY state.CurrentIndex state.Width state.Height
    if state.ImageLayers.Length < layersCount then
        let arr = Array2D.init state.Width state.Height (fun _ _ -> 0)
        arr.[x,y] <- element
        let newLayers = arr :: state.ImageLayers
        let updatedState = { state with CurrentIndex = state.CurrentIndex + 1; ImageLayers = newLayers }
        updatedState
    else
        state.ImageLayers.[0].[x,y] <- element
        let updatedState = { state with CurrentIndex = state.CurrentIndex + 1 }
        updatedState

let calculate value (elements:int[,]) = 
    let rows = elements.GetLength(0) - 1
    let columns = elements.GetLength(1) - 1
    let mutable count = 0
    for row in [0..rows] do
        for column in [0..columns] do          
            if elements.[row, column] = value then 
                count <- count + 1
    count

let pixelFold state element = 
    let previous = snd state
    match (previous, element) with 
    | (2, 2) -> (2, 2)
    | (1, _) -> (1, 1)
    | (0, _) -> (0, 0)
    | (2, 1) -> (1, 1)
    | (2, 0) -> (0, 0)
    | _ -> failwith "Unsupported values"


let getPixelValue x y (imageLayers:int[,] list) =
    let allValues = imageLayers |> List.map (fun layer -> layer.[x,y])
    let pixelValue = allValues |> List.fold pixelFold (2, allValues.[0])
    snd pixelValue

let decodeImage imageLayers width height = 
    let resultImage = Array2D.init width height (fun _ _ -> 2)
    for x in [0..width-1] do
        for y in [0..height-1] do 
            let pixel = getPixelValue x y imageLayers
            resultImage.[x,y] <- pixel
    resultImage

let printImage (image:int[,]) = 
    for y in [0..image.GetLength(1)-1] do
        for x in [0..image.GetLength(0)-1] do 
            let pixel = image.[x,y]
            if pixel = 1 then
                printf "1"
            else printf " "
        printfn ""

let calculateDay8 =
    let inputLines = Utils.readInput "Day8"
    let numbers = Seq.toList inputLines.[0] |> List.map (fun i -> int(Char.GetNumericValue(i)))
    let width = 25
    let height = 6
    let startState = { CurrentIndex = 0; Width = width; Height = height; ImageLayers = [] }
    let imageLayersResult = numbers |> List.fold foldFunction startState
    let calculateZeroDigits = calculate 0
    let result = imageLayersResult.ImageLayers |> List.mapi (fun index layer -> (index, calculateZeroDigits layer)) |> List.minBy (fun element -> snd element)
    let calculateOneDigits = calculate 1
    let calculateTwoDigits = calculate 2
    let applicableLayer = imageLayersResult.ImageLayers.[fst result]
    let resultA = (calculateOneDigits applicableLayer) * (calculateTwoDigits applicableLayer)
    Console.WriteLine("****DAY 8****")
    Console.WriteLine("Part A: " + resultA.ToString())
    
    let allImageLayers = imageLayersResult.ImageLayers |> List.rev
    let image = decodeImage allImageLayers width height
    Console.WriteLine("Part B: ")
    printImage image