module Day10

open System
open System.Threading.Tasks

type Coordinate = { X:int; Y:int}

let processInputToMap (inputLines:string[]) =
    let width = inputLines.[0].Length
    let height = inputLines.Length
    let map = Array2D.init width height (fun _ _ -> '.')
    for x in [0..width-1] do
        for y in [0..height-1] do
            map.[x,y] <- inputLines.[y].[x]
    map

let getVerticalCoordinateLine x startCoord endCoord =
    let result = seq { 
        let startY = startCoord.Y + 1
        let endY = endCoord.Y - 1
        for y in [startY..endY] do 
            yield {X=x;Y=y} 
        }
    result |> Seq.toArray

let getHorizontalCoordinateLine y startCoord endCoord =
    let result = seq { 
        let startX = startCoord.X + 1
        let endX = endCoord.X - 1
        for x in [startX..endX] do 
            yield {X=x;Y=y} 
        }
    result |> Seq.toArray

let getLeftMostCoordinate coordA coordB =
    let xdiff = coordA.X - coordB.X
    if xdiff >= 0 then 
        coordB
    else 
        coordA

let getRightMostCoordinate coordA coordB =
    let xdiff = coordA.X - coordB.X
    if xdiff >= 0 then 
        coordA
    else 
        coordB

let euclidean coordA coordB = 
    let xdiff = coordA.X - coordB.X
    let ydiff = coordA.Y - coordB.Y
    Math.Sqrt( Math.Pow(float xdiff, 2.0) + Math.Pow(float ydiff, 2.0))

let getCoordinateLines coordA coordB =
    let xdiff = coordA.X - coordB.X
    let ydiff = coordA.Y - coordB.Y
    let distance = euclidean coordA coordB
    if distance < 2.0 then 
        let result = [] |> List.toArray
        result
    elif xdiff = 0 && ydiff > 1 then
        let result = getVerticalCoordinateLine coordA.X coordB coordA
        result
    elif xdiff = 0 && ydiff < -1 then
        let result = getVerticalCoordinateLine coordA.X coordA coordB
        result
    elif ydiff = 0 && xdiff > 1 then
       let result = getHorizontalCoordinateLine coordA.Y coordB coordA
       result 
    elif ydiff = 0 && xdiff < -1 then
       let result = getHorizontalCoordinateLine coordA.Y coordA coordB
       result 
    else
        let leftMost = getLeftMostCoordinate coordA coordB
        let RightMost = getRightMostCoordinate coordA coordB
        let leaning = (float (RightMost.Y-leftMost.Y)) / (float (RightMost.X - leftMost.X))
        let result = seq { 
            let startX = leftMost.X + 1
            let endX = RightMost.X - 1
            for x in [startX..endX] do 
                let c = (float (x-leftMost.X) * leaning) + float(leftMost.Y)
                if c = Math.Floor(c) then
                    let candidate = { X=x;Y=int(c) }
                    yield candidate 
            }
        result |> Seq.toArray

let hasAstroidInPath (map:char[,]) path =
    let pathWithAstroids = path |> Array.exists (fun coordinate -> map.[coordinate.X, coordinate.Y] = '#')
    pathWithAstroids

let hasAstroidInPathA astroidlist path =
    let astroidsInPath = Set.intersect (Set.ofList astroidlist) (Set.ofArray path) |> Set.toList
    let pathWithAstroids = astroidsInPath.Length > 0
    pathWithAstroids

let flattenMap (map:char[,]) = 
    let result = seq { 
        let w = map.GetLength(0)
        let h = map.GetLength(1)
        for x in [0..w-1] do 
            for y in [0..h-1] do        
                yield ({X=x;Y=y}, map.[x,y]) 
        }
    result |> Seq.toList

let hasDirectSight coordA coordB map =
    let path = getCoordinateLines coordA coordB
    let hasAstroids = hasAstroidInPath map path
    not hasAstroids

let hasDirectSightA coordA coordB astroidlist =
    let path = getCoordinateLines coordA coordB
    let hasAstroids = hasAstroidInPathA astroidlist path
    not hasAstroids

let calculateDirectSight coordinate map =
    let astroidList = map |> flattenMap 
                          |> List.filter (fun element -> snd element = '#' && fst element <> coordinate)
                          |> List.map (fun element -> fst element)
    let directSightCount = astroidList 
                            |> List.filter (fun element -> hasDirectSight element coordinate map) 
                            |> List.length
    (coordinate, directSightCount)

let getDirectSightAstroids coordinate astroidlist =
    let astroids = astroidlist 
                          |> List.filter (fun element -> element <> coordinate)
                          |> List.filter (fun element -> hasDirectSightA element coordinate astroidlist)
    astroids

let getLaserCoordinates origin (angle:float) maxX maxY =
    let radians = 2.0 * Math.PI * (angle / 360.0)
    let a = Math.Tan(radians)
    let leaning = -1.0 / a
    let result = seq { 
        let startX = if angle < 180.0 then origin.X + 1 else 0
        let endX = if angle < 180.0 then maxX - 1 else origin.X - 1
        for x in [startX..endX] do 
            let nextY = Math.Round((float (x-origin.X) * leaning) + float(origin.Y), 10)
            let a = Math.Abs(nextY - Math.Floor(nextY))
            let b = Math.Abs(nextY - Math.Ceiling(nextY))
            let diff = Math.Min(a , b)
            let maxDiff = 0.05
            if diff < maxDiff && nextY >= (0.0-maxDiff) && nextY < float(maxY) then
                let candidate = { X=x;Y=int(Math.Round(nextY)) }
                yield candidate 
        }
    result |> Seq.toArray

let getCoordinates (origin:Coordinate) angle maxX maxY =
    let degree = float(angle) / 200.0 
    if degree = 0.0 then
        getVerticalCoordinateLine origin.X {origin with Y=(-1)} origin
    elif degree = 180.0 then
        getVerticalCoordinateLine origin.X origin {origin with Y=maxY}
    elif degree = 90.0 then
        getHorizontalCoordinateLine origin.Y origin {origin with X=maxX}
    elif degree = 270.0 then
        getHorizontalCoordinateLine origin.Y {origin with X=(-1)} origin 
    elif degree > 0.0 && degree < 90.0 then
        getLaserCoordinates origin degree maxX maxY
    elif degree > 90.0 && degree < 180.0 then
        getLaserCoordinates origin degree maxX maxY
    elif degree > 180.0 && degree < 270.0 then
        getLaserCoordinates origin degree maxX maxY
    elif degree > 270.0 && degree < 360.0 then
        getLaserCoordinates origin degree maxX maxY
    else 
        failwith "Angle not supported"

let rec remove n lst = 
    match lst with
    | h::tl when h = n -> tl
    | h::tl -> h :: (remove n tl)
    | []    -> []

let copy input =
  let rec copy acc input =
    match input with
    | [] -> List.rev acc
    | x::xs -> copy (x::acc) xs
  copy [] input

let getRadAngle origin coordB =
    let diffX = coordB.X - origin.X
    let diffY = coordB.Y - origin.Y
    if diffX = 0 && diffY = 0 then 0.0
    elif diffX = 0 && diffY > 0 then Math.PI
    elif diffX = 0 && diffY < 0 then 0.0
    elif diffY = 0 && diffX > 0 then Math.PI * 0.5
    elif diffY = 0 && diffX < 0 then Math.PI * 1.5
    elif diffX > 0 && diffY < 0 then
        // Q1
        Math.Atan( float(Math.Abs(diffX)) / float(Math.Abs(diffY)))
    elif diffX > 0 && diffY > 0 then
        // Q2
        let a = Math.PI - Math.Atan(float(Math.Abs(diffX)) / float(Math.Abs(diffY)))
        a
    elif diffX < 0 && diffY > 0 then
        // Q3
        let a = Math.PI + Math.Atan(float(Math.Abs(diffX)) / float(Math.Abs(diffY)))
        a
    else
        // Q4
        let a = 2.0 * Math.PI - Math.Atan(float(Math.Abs(diffX)) / float(Math.Abs(diffY)))
        a
        

let vaporize originLaserPosition astroidlist maxX maxY = 
    let rec loop originLaserPosition maxX maxY (astroidlist:Coordinate list) (directSightAstroids:Coordinate list) (remaining:Coordinate list) (vaporizedlist:Coordinate list) rotationCount angle =
        if vaporizedlist.Length = astroidlist.Length - 1 then
            vaporizedlist |> List.rev
        else

            let currentRotationCount = 
                if (angle + 1) >= 72000 then 
                    rotationCount + 1
                else 
                    rotationCount
            let remainingAstroids = 
                if (angle + 1) >= 72000 then
                    Set.difference (Set.ofList astroidlist) (Set.ofList vaporizedlist) |> Set.toList
                else
                    remaining

            let directSight = 
                if (angle + 1) >= 72000 then
                    getDirectSightAstroids originLaserPosition remainingAstroids
                else
                    directSightAstroids

            let coordinates = getCoordinates originLaserPosition angle maxX maxY |> Array.toList
            //let directSightAstroidsTask = Task.Factory.StartNew (fun () ->getDirectSightAstroids originLaserPosition remainingAstroids)
            let astroidCoordinates = Set.intersect (Set.ofList coordinates) (Set.ofList directSight) |> Set.toList
            let nextAngle = ((angle + 1) % 72000)

            if astroidCoordinates.Length > 0 then
                let (nextCoordinateToVapor,_) = astroidCoordinates 
                                                |> List.map (fun c -> (c, getRadAngle originLaserPosition c)) 
                                                |> List.minBy (fun e -> snd e)
                
                if not(vaporizedlist |> List.contains nextCoordinateToVapor) then
                    let newVaporizedlist = nextCoordinateToVapor :: vaporizedlist
                    loop originLaserPosition maxX maxY astroidlist directSight remainingAstroids newVaporizedlist currentRotationCount nextAngle
                else
                    loop originLaserPosition maxX maxY astroidlist directSight remainingAstroids vaporizedlist currentRotationCount nextAngle
            else
                loop originLaserPosition maxX maxY astroidlist directSight remainingAstroids vaporizedlist currentRotationCount nextAngle

    let directSightAstroids = getDirectSightAstroids originLaserPosition astroidlist
    loop originLaserPosition maxX maxY astroidlist directSightAstroids astroidlist [] 0 0

let calculateDay10 =
    let inputLines = Utils.readInput "Day10"
    let map = processInputToMap inputLines
    let astroidlist = map |> flattenMap 
                          |> List.filter (fun value -> snd value = '#')
                          |> List.map fst
    let resultA = astroidlist 
                     |> List.map (fun value -> calculateDirectSight value map) 
                     |> List.maxBy (fun value -> snd value)


    Console.WriteLine("****DAY 10****")
    Console.WriteLine("Part A: " + resultA.ToString())
    let originLaserPosition = fst resultA
    //let originLaserPosition = {X=11;Y=13}
    let maxX = map.GetLength(0)
    let maxY = map.GetLength(1)
    let vaporized = vaporize originLaserPosition astroidlist maxX maxY |> List.toArray
    let astroidNumber = 200
    let resultB = vaporized.[astroidNumber-1].X * 100 + vaporized.[astroidNumber-1].Y
    Console.WriteLine("Part B: " + resultB.ToString())