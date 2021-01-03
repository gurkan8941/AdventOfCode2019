module Utils

open System
open System.IO

let readInput day = 
    let filePath =  Path.Combine(Environment.CurrentDirectory, day, "input.txt")
    let lines = File.ReadAllLines filePath
    lines

