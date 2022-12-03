// Learn more about F# at http://fsharp.org

open System

open System.IO

type curr =
| Progress of int
| Final of int

let get_val line curr =
    match line with
    | "" -> Final curr
    | i -> Progress (curr + (Int32.Parse(line)))

let rec part_1 (sr: StreamReader) max curr =
    match sr.EndOfStream with
    | true ->
        let max = if curr > max then curr else max
        printfn "FOUND MAX: %d" max
    | false ->
        let line = sr.ReadLine()
        match get_val line curr with
        | Progress curr ->
            part_1 sr max curr
        | Final curr ->
            let max = if curr > max then curr else max
            part_1 sr max 0

let rec insert_aux (l: int list) (el: int) (r: int list) (index: int) : int list =
    if index >= 3 then List.rev r
    else
        match l with
        | [] -> List.rev (el :: r)
        | head :: l ->
            if head > el then insert_aux l el (head :: r) (index + 1)
            else insert_aux l head (el :: r) (index + 1)

let insert l el = insert_aux l el [] 0

let rec part_2 (sr: StreamReader) l curr =
    match sr.EndOfStream with
    | true ->
        let l = insert l curr
        let total = List.fold (fun sum el -> sum + el) 0 l
        printfn "FOUND TOTAL: %d" total
    | false ->
        let line = sr.ReadLine()
        match get_val line curr with
        | Progress curr ->
            part_2 sr l curr
        | Final curr ->
            let l = insert l curr
            part_2 sr l 0

[<EntryPoint>]
let main argv =
    use sr = new StreamReader ("input")
    // part_1 sr 0 0

    part_2 sr [] 0

    0 // return an integer exit code
