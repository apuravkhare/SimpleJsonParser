let solve path =
    printfn "File: %s" path
    let parsed = Parser.parseFile path
    printfn "Parsed: %A" (Expression.prettyPrint parsed)
    let (var, exp) = Expression.simplify parsed
    printfn "Simplified: %s = %A" var (Expression.prettyPrint exp)
    let solution = Expression.solve (var, exp)
    printfn "Solution: %A" (Expression.prettyPrint solution)

[<EntryPoint>]
let main argv =
    if argv |> Array.isEmpty then
        solve @".\sample.json"
    else
        solve (argv.[0])
    0 // return an integer exit code
