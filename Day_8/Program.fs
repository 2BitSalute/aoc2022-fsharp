open System

open System.IO

let rec is_visible (map: string array) h x y dx dy =
    if x = 0 || y = 0 || x = map.Length - 1 || y = map.[0].Length - 1 then
        true
    else
        let x' = x + dx
        let y' = y + dy
        if map.[y'].[x'] >= h then
            false
        else
            is_visible map h x' y' dx dy

let rec solve_p1 (map: string array) x y count =
    let count =
        if  is_visible map map.[y].[x] x y 0 1 ||
            is_visible map map.[y].[x] x y 1 0 ||
            is_visible map map.[y].[x] x y 0 (-1) ||
            is_visible map map.[y].[x] x y (-1) 0 then
            count + 1
        else
            count
    let x' = x + 1
    let y' = y + 1
    if x' < map.[y].Length then
        solve_p1 map x' y count
    else if y' < map.Length then
        solve_p1 map 0 y' count
    else
        count

let rec view (map: string array) h x y dx dy count =
    if x = 0 || y = 0 || x = map.Length - 1 || y = map.[0].Length - 1 then
        count
    else
        let x' = x + dx
        let y' = y + dy
        if map.[y'].[x'] >= h then
            count + 1
        else
            view map h x' y' dx dy (count + 1)

let rec solve_p2 (map: string array) x y m =
    let count =
            view map map.[y].[x] x y 0 1 0 *
            view map map.[y].[x] x y 1 0 0 *
            view map map.[y].[x] x y 0 (-1) 0 *
            view map map.[y].[x] x y (-1) 0 0

    let m = if m < count then count else m
    let x' = x + 1
    let y' = y + 1
    if x' < map.[y].Length then
        solve_p2 map x' y m
    else if y' < map.Length then
        solve_p2 map 0 y' m
    else
        m

[<EntryPoint>]
let main argv =
    use sr = new StreamReader ("input")

    let rec read_lines l =
        match sr.EndOfStream with
        | true -> List.rev l |> Array.ofList
        | false -> read_lines (sr.ReadLine() :: l)

    let map = read_lines []

    let result = solve_p2 map 0 0 0

    printfn "RESULT: %d" result

    0 // return an integer exit code
