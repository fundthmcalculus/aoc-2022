module Utilities

open System
open System.IO

let readLines filePath = File.ReadLines(filePath);
let readFile(day: int, demo: bool): seq<string> = 
    let demoSuffix = if demo then "_demo" else "";
    readLines(Path.Combine("data",$"day{day}{demoSuffix}.txt"))
    
let topN(source: seq<_>, N: int): seq<_> =
    let lowHighSort = Seq.sort source
    let highLowSort = Seq.rev lowHighSort
    Seq.take N highLowSort
    
let printSequence(source: seq<_>): string =
    (", ", source  |> Seq.map(string)) |> String.Join
    
let flipMap(map: Map<_,_>): Map<_,_> =
    Map.ofSeq(seq {
        for key in map.Keys do yield map[key], key
    })