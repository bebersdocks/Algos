module AlgosTests.TestUtils

open System

open Xunit

let haveSameContent xs ys =
    let rec haveSameContent xs ys =
        match xs, ys with
        | [], [] -> true
        | x :: xs, y :: ys when x = y -> haveSameContent xs ys
        | _ -> false
    Assert.True(haveSameContent xs ys)

let produceSameContent (xs: 'a list) (f: 'a list -> 'a list) (g: 'a list -> 'a list) =
    haveSameContent (f xs) (g xs)

let equal (xs: 'a list) (f: 'a list -> 't) (g: 'a list -> 't) =
    Assert.Equal<'t>(f xs, g xs)

let nextRandom min max =
    Random().Next(min, max)

/// <summary>
/// Generates list of pseudo-random length and content.
/// </summary>
let randomListOfFunc f min max =
    seq { 0 .. nextRandom min max }
    |> Seq.map f
    |> List.ofSeq

let nextRandomNumbers () =
    randomListOfFunc (fun _ -> nextRandom -1000 1000) 5 60

let nextLists =
    randomListOfFunc (fun _ -> nextRandomNumbers ()) 10 30
