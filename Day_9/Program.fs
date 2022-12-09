open System

open System.IO

open System.Collections.Generic

type coordinates = (int * int)

[<EntryPoint>]
let main argv =
    use sr = new StreamReader ("small_input")

    let visited = Set<coordinates>([])

    let make_direction token =
        match token with
        | "R" -> (1, 0)
        | "L" -> (-1, 0)
        | "U" -> (0, -1)
        | "D" -> (0, 1)
        | _ -> failwith "Unknown direction"

    let rec move visited (hx, hy) (tx, ty) (dx, dy) magnitude =
        if magnitude = 0 then
            ((hx, hy), (tx, ty), visited)
        else
            let hx = hx + dx
            let hy = hy + dy
            // move the tail
            let (dtx, dty) =
                match (hx - tx, hy - ty) with
                | (2, 0) -> (1, 0)
                | (0, 2) -> (0, 1)
                | (-2, 0) -> (-1, 0)
                | (0, -2) -> (0, -1)
                | (2, 1) | (1, 2) -> (1, 1)
                | (-2, 1) | (-1, 2) -> (-1, 1)
                | (1, -2) | (2, -1) -> (1, -1)
                | (-2, -1) | (-1, -2) -> (-1, -1)
                | _ -> (0, 0)
            let (tx, ty) = (tx + dtx, ty + dty)
            let visited = Set.add (tx, ty) visited
            move visited (hx, hy) (tx, ty) (dx, dy) (magnitude - 1)

    let rec solve_p1 visited (hx, hy) (tx, ty) =
        match sr.EndOfStream with
        | true -> Set.count visited
        | false ->
            let tokens = (sr.ReadLine()).Split()
            let direction = make_direction(tokens.[0])
            let magnitude = Int32.Parse(tokens.[1])
            let ((hx, hy), (tx, ty), visited) = move visited (hx, hy) (tx, ty) direction magnitude
            solve_p1 visited (hx, hy) (tx, ty)

    let result = solve_p1 visited (0, 0) (0, 0)

    printfn "RESULT: %d" result

    0 // return an integer exit code