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

    let top3 =
        function
        | x -> topN (x, 3)

    printfn $"1A: %d{readFile (1, demo) |> countCalories |> Seq.max}"

    printfn
        $"1B: %d{readFile (1, demo)
                 |> countCalories
                 |> top3
                 |> Seq.sum}"

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

    printfn
        $"2A: {readFile (2, demo)
               |> Seq.map (playLine)
               |> Seq.sum}"

    printfn
        $"2B: {readFile (2, demo)
               |> Seq.map (strategyLine)
               |> Seq.sum}"


printfn "AOC 2022"
day1 ()
day2 ()
