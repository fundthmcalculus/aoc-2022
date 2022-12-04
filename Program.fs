module aoc_2022

open System
open Utilities

let countCalories (lines: seq<string>) : seq<int> =
    let mutable calorieSum = 0

    seq {
        for line in lines do
            if String.IsNullOrEmpty(line) then
                yield calorieSum
                calorieSum <- 0
            else
                calorieSum <- calorieSum + int (line)

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

    printfn $"1A: %d{lines |> countCalories |> Seq.max}"

    printfn $"1B: %d{lines |> countCalories |> top3 |> Seq.sum}"

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
    scorePlay (myPlay)
    + 3 * (1 + playRound (myPlay, opponentPlay))

let playLine (line: string) : int =
    let plays =
        (line.Split " ") |> Seq.map (namePlay)

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

    printfn $"2A: {lines |> Seq.map (playLine) |> Seq.sum}"

    printfn $"2B: {lines |> Seq.map (strategyLine) |> Seq.sum}"


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
     |> Seq.map (uniqueChars)
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
        $"3A: {lines
               |> Seq.map (splitRucksack)
               |> Seq.map (commonCharacter)
               |> Seq.map (prioritize)
               |> Seq.sum}"

    printfn
        $"3B: {lines
               |> Seq.chunkBySize (3)
               |> Seq.map (commonBadge)
               |> Seq.map (prioritize)
               |> Seq.sum}"


printfn "AOC 2022"
day1 ()
day2 ()
day3 ()
