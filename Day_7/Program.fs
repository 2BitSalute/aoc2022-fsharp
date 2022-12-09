open System

open System.IO

[<EntryPoint>]
let main argv =
    use sr = new StreamReader ("input")
    let line = sr.ReadLine()

    printfn "LINE: %s" line

    let rec outer curr start =
        let rec inner p counter start =
            if p < start || line.[p] = line.[curr] then (p + 1, counter)
            else inner (p - 1) (counter + 1) start
        let (start, counter) = inner (curr - 1) 1 start
        if counter = 14 then curr + 1
        else outer (curr + 1) start

    let result = outer 1 0

    printfn "RESULT: %d" result

    0 // return an integer exit code
