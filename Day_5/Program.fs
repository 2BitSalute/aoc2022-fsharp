open System

open System.IO

let rec read_crates (line: string) (i: int) (stacks: char list array) (si: int) =
    if line.Length <= i then ()
    else
        // printfn "l: %d i: %d" line.Length (i + 1)
        match line.[i + 1] with
        | ' ' -> ()
        | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
            let si = int (Char.GetNumericValue line.[i + 1]) - 1
            stacks.[si] <- List.rev stacks.[si]
            ()
        | c ->
            // printfn "si == %d" si
            stacks.[si] <- c :: stacks.[si]
        read_crates line (i + 4) stacks (si + 1)

let rec read_start (sr: StreamReader) (stacks: char list array) =
    match sr.EndOfStream with
    | true -> ()
    | false ->
        let line = sr.ReadLine()
        match line with
        | "" -> ()
        | line ->
            read_crates line 0 stacks 0
            read_start sr stacks

let rec move (stacks: char list array) count f t =
    if count = 0 then ()
    else
        printfn "Move %d from %d to %d" count f t
        match stacks.[f] with
        | [] ->
            failwith "OOPS"
        | x :: rest ->
            stacks.[f] <- rest
            stacks.[t] <- x :: stacks.[t]
            move stacks (count - 1) f t

let move2 (stacks: char list array) count f t =
    let rec aux count f t l =
        if count = 0 then l
        else
            printfn "Move %d from %d to %d" count f t
            match stacks.[f] with
            | [] ->
                failwith "OOPS"
            | x :: rest ->
                stacks.[f] <- rest
                aux (count - 1) f t (x :: l)

    let l = aux count f t []
    List.iter (fun el -> stacks.[t] <- el :: stacks.[t]) l

let print_stacks (stacks: char list array) =
    let rec aux (stacks: char list array) si l =
        if si = l then ()
        else
            List.iter (fun c -> printf "%c" c) stacks.[si]
            printfn ""
            aux stacks (si + 1) l
    aux stacks 0 stacks.Length
    printfn ""

let print_tops_of_stacks (stacks: char list array) =
    let rec aux (stacks: char list array) si l =
        if si = l then ()
        else
            printfn "%c" stacks.[si].Head
            aux stacks (si + 1) l
    aux stacks 0 stacks.Length
    printfn ""

let rec solve (sr: StreamReader) (stacks: char list array) mover =
    match sr.EndOfStream with
    | true -> ()
    | false ->
        let line = sr.ReadLine()
        let tokens = line.Split()
        let count = Int32.Parse tokens.[1]
        let f = Int32.Parse tokens.[3]
        let t = Int32.Parse tokens.[5]
        mover stacks count (f - 1) (t - 1)
        print_stacks stacks

        solve sr stacks mover

[<EntryPoint>]
let main argv =
    use sr = new StreamReader ("input")

    // Small input (length - 1)
    let n = 8
    let stacks = [| for i in 0 .. n -> [] |]

    read_start sr stacks
    print_stacks stacks

    // Swallow empty line
    // let line = sr.ReadLine ()

    solve sr stacks move2

    printfn "Stack 1: %d Stack 2: %d Stack 3: %d" (List.length stacks.[0]) (List.length stacks.[1]) (List.length stacks.[2])

    print_tops_of_stacks stacks

    0 // return an integer exit code
