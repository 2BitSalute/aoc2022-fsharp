// Learn more about F# at http://fsharp.org

open System

open System.IO

let map_p1 v =
    match v with
    | "A"
    | "X" -> 1
    | "B"
    | "Y" -> 2
    | "C"
    | "Z" -> 3
    | other -> failwith ("Unexpected value" + other)

type outcome =
    | Loss
    | Draw
    | Win

let map_p2 v =
    match v with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | other -> failwith ("Unexpected value" + other)

let calc_p1 left right =
    match left, right with
    | 1, 2
    | 2, 3
    | 3, 1
      -> 6
    | 1, 1
    | 2, 2
    | 3, 3
      -> 3
    | _, _ -> 0

let calc_p2 left right =
    let v =
        match right with
        | Loss -> (left - 1) % 3
        | Draw -> left
        | Win -> (left + 1) % 3
    if v = 0 then 3
    else v

let rec part_1 (sr: StreamReader) sum =
    match sr.EndOfStream with
    | true ->
        sum
    | false ->
        let line = sr.ReadLine()
        let tokens = line.Split()

        let opponent = map_p1 (tokens.[0])
        let you = map_p1 (tokens.[1])

        let turn_score = calc_p1 opponent you
        part_1 sr (sum + turn_score + you)

let rec part_2 (sr: StreamReader) sum =
    match sr.EndOfStream with
    | true ->
        sum
    | false ->
        let line = sr.ReadLine()
        let tokens = line.Split()

        let opponent = map_p1 (tokens.[0])
        let you = calc_p2 opponent (map_p2 (tokens.[1]))
        printfn "OPPONENT: %d YOU: %d" opponent you

        let turn_score = calc_p1 opponent you
        part_2 sr (sum + turn_score + you)

[<EntryPoint>]
let main argv =
    use sr = new StreamReader ("input")

    let result = part_2 sr 0

    printfn "RESULT: %d" result

    0 // return an integer exit code
