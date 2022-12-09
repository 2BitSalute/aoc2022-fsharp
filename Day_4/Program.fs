// Learn more about F# at http://fsharp.org

open System

open System.IO

let is_completely_contained (left: string array) (right: string array) =
    let s1 = Int32.Parse(left.[0])
    let s2 = Int32.Parse(right.[0])
    let e1 = Int32.Parse(left.[1])
    let e2 = Int32.Parse(right.[1])

    if  (s1 >= s2 && e1 <= e2) ||
        (s2 >= s1 && e2 <= e1) then
        true
    else
        false

let is_overlapping (left: string array) (right: string array) =
    let s1 = Int32.Parse(left.[0])
    let s2 = Int32.Parse(right.[0])
    let e1 = Int32.Parse(left.[1])
    let e2 = Int32.Parse(right.[1])

    if e1 >= s2 && s1 <= s2 then
        // printfn "e1 %d >= s2 %d && s1 %d <= s2 %d" e1 s2 s1 s2
        true
    else if e2 >= s1 && s2 <= s1 then
        // printfn "e2 %d >= s1 %d && s2 %d <= s1 %d" e2 s1 s2 s1
        true
    else
        false

let rec part_1 (sr: StreamReader) sum =
    match sr.EndOfStream with
    | true ->
        sum
    | false ->
        let line = sr.ReadLine()
        let ranges = line.Split(',')

        if is_completely_contained (ranges.[0].Split('-')) (ranges.[1].Split('-')) then
            part_1 sr (sum + 1)
        else
            part_1 sr sum

let rec part_2 (sr: StreamReader) sum =
    match sr.EndOfStream with
    | true ->
        sum
    | false ->
        let line = sr.ReadLine()
        let ranges = line.Split(',')

        if is_overlapping (ranges.[0].Split('-')) (ranges.[1].Split('-')) then
            part_2 sr (sum + 1)
        else
            part_2 sr sum

[<EntryPoint>]
let main argv =
    use sr = new StreamReader ("input")

    let result = part_2 sr 0

    printfn "RESULT: %d" result

    0 // return an integer exit code
