module Utilities

open System
open System.IO

let readLines filePath = File.ReadLines(filePath)

let readFile (day: int, demo: bool) : seq<string> =
    let demoSuffix =
        if demo then "_demo" else ""

    readLines (Path.Combine("data", $"day{day}{demoSuffix}.txt"))

let topN (source: seq<_>, N: int) : seq<_> =
    Seq.sort source |> Seq.rev |> Seq.take N

let printSequence (source: seq<_>) : string =
    (", ", source |> Seq.map string) |> String.Join

let flipMap (map: Map<_, _>) : Map<_, _> =
    seq {
        for key in map.Keys do
            yield (map[key], key)
    }
    |> Map.ofSeq
