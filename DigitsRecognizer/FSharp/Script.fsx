// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open FSharp

// Define your library scripting code here
let square x = x * x
let sumOfSquares n = [1..n] |> List.map square |> List.sum

let answer = sumOfSquares 100

