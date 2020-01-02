module Helper

open System

let coalesceString lst =
    lst
    |> List.tryPick ( fun s -> if String.IsNullOrEmpty(s) then None else Some(s) )
    |> Option.defaultValue ""