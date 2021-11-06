module AlgosTests

open ListUtils
open Sorting
open System
open Xunit

let nextRandom minValue maxValue = 
    Random().Next(minValue, maxValue)

///<summary>
/// Generates list of pseudo-random length and content.
///</summary>
let nextRandomNumbers =
    seq { 0 .. nextRandom 5 60 }
    |> Seq.map (fun _ -> nextRandom -1000 1000)
    |> List.ofSeq

let lists =
    seq { 0 .. nextRandom 10 30 }
    |> Seq.map (fun _ -> nextRandomNumbers)
    |> List.ofSeq

let equal (xs: 'a list) (f: 'a list -> 't) (g: 'a list -> 't) =
    Assert.Equal<'t>(f xs, g xs)

let produceSameContent (xs: 'a list) (f: 'a list -> 'a list) (g: 'a list -> 'a list) =
    let rec produceSameContent xs ys =
        match xs, ys with
        | [], [] -> true
        | x :: xs, y :: ys when x = y -> produceSameContent xs ys
        | _ -> false
    Assert.True(produceSameContent (f xs) (g xs))

[<Fact>]
let ``Length of the list`` () =
    lists
    |> List.iter (fun x -> equal x ListUtils.length List.length)

[<Fact>]
let ``Insertion sort`` () =
    lists
    |> List.iter (fun x -> produceSameContent x insertionSort List.sort)

[<Fact>]
let ``Quick sort`` () =
    lists
    |> List.iter (fun x -> produceSameContent x quickSort List.sort)

[<Fact>]
let ``Bubble sort`` () =
    lists
    |> List.iter (fun x -> produceSameContent x bubbleSort List.sort)
