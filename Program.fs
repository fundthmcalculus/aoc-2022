module aoc_2022

open System
open System.Collections.Generic
open Utilities

let countCalories (lines: seq<string>) : seq<int> =
    let mutable calorieSum = 0

    seq {
        for line in lines do
            if String.IsNullOrEmpty(line) then
                yield calorieSum
                calorieSum <- 0
            else
                calorieSum <- calorieSum + int line

        for x in Seq.singleton calorieSum do
            yield x
    }

let day1 () =
    let demo = false
    let day = 1
    let lines = readFile (day, demo)

    let top3 =
        function
        | x -> topN (x, 3)

    printfn $"{day}A: %d{lines |> countCalories |> Seq.max}"

    printfn $"{day}B: %d{lines |> countCalories |> top3 |> Seq.sum}"

let winMap =
    Map [ "rock", "scissors"
          "paper", "rock"
          "scissors", "paper" ]

let lossMap = flipMap winMap

let playMap =
    Map [ "A", "rock"
          "X", "rock"
          "B", "paper"
          "Y", "paper"
          "C", "scissors"
          "Z", "scissors" ]

let scoreMap =
    Map [ "rock", 1
          "paper", 2
          "scissors", 3 ]

let namePlay (x: string) : string = playMap[x]

let playRound (myPlay: string, opponentPlay: string) : int =
    if myPlay.Equals(opponentPlay) then
        0
    else if winMap[ myPlay ].Equals(opponentPlay) then
        1
    else
        -1

let scorePlay (play: string) : int = scoreMap[play]

let scoreRound (myPlay: string, opponentPlay: string) : int =
    scorePlay myPlay
    + 3 * (1 + playRound (myPlay, opponentPlay))

let playLine (line: string) : int =
    let plays =
        (line.Split " ") |> Seq.map namePlay

    scoreRound (Seq.last plays, Seq.head plays)

let strategyPlay (strategy: string, opponentPlay: string) =
    if strategy.Equals("X") then
        winMap[opponentPlay]
    else if strategy.Equals("Y") then
        opponentPlay
    else
        lossMap[opponentPlay]

let strategyLine (line: string) : int =
    let plays = line.Split " "
    let opponentPlay = namePlay (Seq.head plays)

    let myPlay =
        strategyPlay ((Seq.last plays), opponentPlay)

    scoreRound (myPlay, opponentPlay)

let day2 () =
    let demo = false
    let day = 2
    let lines = readFile (day, demo)

    printfn $"{day}A: {lines |> Seq.map playLine |> Seq.sum}"

    printfn $"{day}B: {lines |> Seq.map strategyLine |> Seq.sum}"


let splitRucksack (line: string) : string * string =
    let N = line.Length / 2
    let a = line.Substring(0, N)
    let b = line.Substring(N, N)
    a, b

let uniqueChars (a: string) : Set<char> = Set.ofArray (a.ToCharArray())

let commonCharacter (a: string, b: string) : char =
    let setA = uniqueChars a
    let setB = uniqueChars b
    ((Set.intersect setA setB) |> Set.toList).Head


let commonBadge (a: string []) : char =
    (Seq.ofArray a
     |> Seq.map uniqueChars
     |> Set.intersectMany
     |> Set.toList)
        .Head



let prioritize (c: char) : int =
    if Char.IsLower(c) then
        int c - int 'a' + 1
    else
        int c - int 'A' + 27

let day3 () =
    let demo = false
    let day = 3
    let lines = readFile (day, demo)

    printfn
        $"{day}A: {lines
               |> Seq.map splitRucksack
               |> Seq.map commonCharacter
               |> Seq.map prioritize
               |> Seq.sum}"

    printfn
        $"{day}B: {lines
               |> Seq.chunkBySize 3
               |> Seq.map commonBadge
               |> Seq.map prioritize
               |> Seq.sum}"
               

let toRange(x: string): Range =
    let y = Seq.ofArray(x.Split("-")) |> Seq.map(int) |> Seq.toArray
    Range(y[0],y[1])

let findRange(line: string): Range*Range =
    let x = Seq.ofArray(line.Split(",")) |> Seq.map(toRange) |> Seq.toArray
    x[0], x[1]

let fullyContained(x: Range, y: Range): bool =
    x.Start.Value <= y.Start.Value && x.End.Value >= y.End.Value
    
let oneFullyContains(x: Range, y: Range): bool =
    fullyContained(x, y) || fullyContained(y, x)
    
let rangeIntersect(x: Range, y: Range): bool =
    not (x.End.Value < y.Start.Value || x.Start.Value > y.End.Value)
    
let day4 () =
    let demo = false
    let day = 4
    let lines = readFile (day, demo)

    printfn
        $"{day}A: {lines
               |> Seq.map findRange
               |> Seq.map oneFullyContains
               |> Seq.map Convert.ToInt32
               |> Seq.sum}"

    printfn
        $"{day}B: {lines
               |> Seq.map findRange
               |> Seq.map rangeIntersect
               |> Seq.map Convert.ToInt32
               |> Seq.sum}"
         
let getRowOfBoxes(line: string): seq<int*string> =
    Seq.ofArray(line.ToCharArray()) |> Seq.chunkBySize(4) |> Seq.map(fun x -> String.Join("", x)) |> Seq.mapi(fun i x -> i, x.Trim())

let pushBox(stackZip: string*Stack<string>) =
    snd(stackZip).Push(fst(stackZip))
    snd(stackZip)

let day5 () =
    let demo = true
    let day = 5
    let lines = readFile (day, demo)
    let blankLineIndex = lines |> Seq.findIndex(String.IsNullOrWhiteSpace)
    let instructionLines = lines |> Seq.skip(blankLineIndex+1)
    let stackLines = lines |> Seq.take(blankLineIndex-1)
    let stackIndex = lines |> Seq.skip(blankLineIndex-1) |> Seq.head
    // Create the sequence of stacks
    let stackCount = stackIndex.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.ofArray |> Seq.last |> int
    let stacks = seq { for _ij in 1..stackCount do Stack<string>(5) }
    
    // Populate the stacks - back to front because stacks
    let allBoxes: seq<seq<string>> = stackLines |> Seq.map(getRowOfBoxes) |> Seq.concat |> Seq.groupBy(fst) |> 

    printfn
        $"{day}A: {instructionLines
               |> Seq.map findRange
               |> Seq.map oneFullyContains
               |> Seq.map Convert.ToInt32
               |> Seq.sum}"

    printfn
        $"{day}B: {lines
               |> Seq.map findRange
               |> Seq.map rangeIntersect
               |> Seq.map Convert.ToInt32
               |> Seq.sum}"


printfn "AOC 2022"
let t0 = DateTime.Now
day1 ()
day2 ()
day3 ()
day4 ()
day5 ()
let t1 = DateTime.Now
printfn $"Elapsed Time={t1-t0}"
