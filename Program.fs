module aoc_2022

open System
open Utilities

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

let day1() =
    let demo = false
    printfn $"1A: %d{Seq.max(countCalories(readFile(1, demo)))}"
    printfn $"1B: %d{Seq.sum(topN(countCalories(readFile(1, demo)), 3))}"
    
    
let winMap = Map ["rock", "scissors"; "paper", "rock"; "scissors", "paper";]
let lossMap = flipMap winMap
let playMap = Map [ "A", "rock"; "X", "rock"; "B", "paper"; "Y", "paper"; "C", "scissors"; "Z", "scissors"; ]
let namePlay(x: string): string =
    playMap[x]
    
let playRound(myPlay: string, opponentPlay: string): int =
    if myPlay.Equals(opponentPlay) then
        0
    else if winMap[myPlay].Equals(opponentPlay) then
        1
    else
        -1
    
let scorePlay(play: string): int =
    let scoreMap = Map ["rock", 1; "paper", 2; "scissors", 3]
    scoreMap[play]

let scoreRound(myPlay: string, opponentPlay: string): int =
    scorePlay(myPlay) + 3*(1+playRound(myPlay,opponentPlay))
    
let playLine(line: string): int =
    let plays = (line.Split " ") |> Seq.map(namePlay)
    scoreRound(Seq.last plays, Seq.head plays)
    
let strategyPlay(strategy: string, opponentPlay: string) =
    if strategy.Equals("X") then
        winMap[opponentPlay]
    else if strategy.Equals("Y") then
        opponentPlay
    else lossMap[opponentPlay]
    
let strategyLine(line: string): int =
    let plays = line.Split " "
    let opponentPlay = namePlay (Seq.head plays)
    let myPlay = strategyPlay((Seq.last plays), opponentPlay)
    scoreRound(myPlay, opponentPlay)
    
let playFile(lines: seq<string>, strategy: (string -> int)): seq<int> =
    lines |> Seq.map(strategy)
    
let day2() =
    let demo = false
    printfn $"2A: {Seq.sum(playFile(readFile(2, demo), playLine))}"
    printfn $"2B: {Seq.sum(playFile(readFile(2, demo), strategyLine))}"


printfn "AOC 2022"
day1()
day2()