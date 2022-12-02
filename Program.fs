// For more information see https://aka.ms/fsharp-console-apps

open System
open System.IO

printfn "Advent of Code"

let readLines filePath = File.ReadLines(filePath);
let readFile(day: int, demo: bool): seq<string> = 
    let demoSuffix = if demo then "_demo" else "";
    readLines(Path.Combine("data",$"day{day}{demoSuffix}.txt"))
    
// let chunkSequence(self: seq<string>, split: string): seq<seq<string>> =
//     let currentSequence = Seq.empty<string>
//     
//     for x: string in self do
//         if x.Equals(split) then
//             yield currentSequence
//             currentSequence = []
//         else
//             currentSequence = seq { yield! currentSequence; yield x }

let countCalories(lines: seq<string>): seq<int> =
    let mutable calorieSum = 0
    seq {
        for line in lines do
            if String.IsNullOrEmpty(line) then
                yield calorieSum
                calorieSum <- 0
            else
                calorieSum <- calorieSum + int(line)
        for x in Seq.singleton calorieSum do
            yield x
        }
    
let topN(source: seq<_>, N: int): seq<_> =
    let lowHighSort = Seq.sort source
    let highLowSort = Seq.rev lowHighSort
    Seq.take N highLowSort
    
let printSequence(source: seq<_>): string =
    (", ", source) |> String.Join

let day1() =
    let demo = false
    printfn $"%d{Seq.max(countCalories(readFile(1, demo)))}"
    printfn $"%d{Seq.sum(topN(countCalories(readFile(1, demo)), 3))}"