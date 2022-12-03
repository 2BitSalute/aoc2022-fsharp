// Learn more about F# at http://fsharp.org

open System

open System.IO

// Lowercase item types a through z have priorities 1 through 26.
// Uppercase item types A through Z have priorities 27 through 52.

let priority c =
    if int c < int 'a' then
        int c - int 'A' + 26
    else
        int c - int 'a'

let rec fill (input: string) index (a: int array) =
    if input.Length = index then ()
    else
        let p = priority input.[index]
        // printfn "char: `%c` p: %d" input.[index] p
        a.[p] <- a.[p] + 1
        fill input (index + 1) a

let rec calc index (a1: int array) (a2: int array) sum =
    if a1.Length = index then sum
    else if a1.[index] > 0 && a2.[index] > 0 then
        printfn "    %d occurs in both compartments" (index + 1)
        calc (index + 1) a1 a2 (sum + index + 1)
    else calc (index + 1) a1 a2 sum

let rec part_1 (sr: StreamReader) sum =
    match sr.EndOfStream with
    | true ->
        sum
    | false ->
        let line = sr.ReadLine()

        let compartment_length = line.Length / 2

        let c1 = line.[0 .. compartment_length - 1]
        let c2 = line.[compartment_length .. line.Length - 1]

        printfn "C1: %s" c1
        printfn "C2: %s" c2

        let a1 = (Array.create 52 0)
        let a2 = (Array.create 52 0)

        fill c1 0 a1
        fill c2 0 a2

        let csum = calc 0 a1 a2 0

        part_1 sr (sum + csum)

let rec calc_p2 index (a1: int array) (a2: int array) (a3: int array) sum =
    if a1.Length = index then sum
    else if a1.[index] > 0 && a2.[index] > 0 && a3.[index] > 0 then
        printfn "    %d occurs in 3 bags" (index + 1)
        calc_p2 (index + 1) a1 a2 a3 (sum + index + 1)
    else calc_p2 (index + 1) a1 a2 a3 sum

let rec part_2 (sr: StreamReader) sum =
    match sr.EndOfStream with
    | true ->
        sum
    | false ->
        let line1 = sr.ReadLine()
        let a1 = (Array.create 52 0)
        fill line1 0 a1

        let line2 = sr.ReadLine()
        let a2 = (Array.create 52 0)
        fill line2 0 a2

        let line3 = sr.ReadLine()
        let a3 = (Array.create 52 0)
        fill line3 0 a3

        let csum = calc_p2 0 a1 a2 a3 0

        part_2 sr (sum + csum)

[<EntryPoint>]
let main argv =
    use sr = new StreamReader ("input")

    let result = part_2 sr 0

    printfn "RESULT: %d" result

    0 // return an integer exit code
