module SortingTests

open System

open ListUtils

open Sorting

open Xunit

let nextRandom min max = 
    Random().Next(min, max)

/// <summary>
/// Generates list of pseudo-random length and content.
/// </summary>
let randomListOfFunc f min max =
    seq { 0 .. nextRandom min max }
    |> Seq.map f
    |> List.ofSeq

let nextRandomNumbers =
    randomListOfFunc (fun _ -> nextRandom -1000 1000) 5 60

let nextLists =
    randomListOfFunc (fun _ -> nextRandomNumbers) 10 30

let equal (xs: 'a list) (f: 'a list -> 't) (g: 'a list -> 't) =
    Assert.Equal<'t>(f xs, g xs)

[<Fact>]
let ``Length of the list`` () =
    nextLists
    |> List.iter (fun x -> equal x ListUtils.length List.length)

let produceSameContent (xs: 'a list) (f: 'a list -> 'a list) (g: 'a list -> 'a list) =
    let rec produceSameContent xs ys =
        match xs, ys with
        | [], [] -> true
        | x :: xs, y :: ys when x = y -> produceSameContent xs ys
        | _ -> false
    Assert.True(produceSameContent (f xs) (g xs))
 
let sortIsCorrect f =
    nextLists
    |> List.iter (fun x -> produceSameContent x f List.sort)

[<Fact>]
let ``Insertion sort`` () =
    sortIsCorrect insertionSort

[<Fact>]
let ``Quick sort`` () =
    sortIsCorrect quickSort

[<Fact>]
let ``Bubble sort`` () =
    sortIsCorrect bubbleSort

[<Fact>]
let ``Selection sort`` () =
    sortIsCorrect selectionSort
