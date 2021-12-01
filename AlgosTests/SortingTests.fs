module AlgosTests.SortingTests

open System

open Algos.ListUtils
open Algos.Sorting

open AlgosTests.TestUtils

open Xunit

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

[<Fact>]
let ``Merge sort`` () =
    sortIsCorrect mergeSort
