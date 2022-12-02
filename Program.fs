// For more information see https://aka.ms/fsharp-console-apps

open System.IO

printfn "Advent of Code"

let readLines filePath = File.ReadLines(filePath);
let readFile(day: int, demo: bool): seq<string> = 
    let demoSuffix = if demo then "_demo" else "";
    readLines($"day{day}{demoSuffix}.txt")
    
// let chunkSequence(self: seq<string>, split: string): seq<seq<string>> =
//     let currentSequence = Seq.empty<string>
//     
//     for x: string in self do
//         if x.Equals(split) then
//             yield currentSequence
//             currentSequence = []
//         else
//             currentSequence = seq { yield! currentSequence; yield x }
let day1A() =
    Seq.max(for line: string in readFile(1, true) do
        )