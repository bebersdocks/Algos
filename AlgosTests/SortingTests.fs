module AlgosTests.SortingTests

open System

open Algos.ListUtils
open Algos.Sorting

open AlgosTests.TestUtils

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

[<Fact>]
let ``Length of the list`` () =
    nextLists
    |> List.iter (fun x -> equal x Algos.ListUtils.length List.length)

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
