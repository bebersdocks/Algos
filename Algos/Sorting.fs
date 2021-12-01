module Algos.Sorting

open Algos.ListUtils

let insertionSort xs =
    let rec insert x ys =
        match ys with
        | [] -> [x]
        | hd :: tail when x <= hd -> x :: hd :: tail
        | hd :: tail -> hd :: (insert x tail)
    let rec insertionSort xs acc =
        match xs with
        | [] -> acc
        | hd :: tail -> insertionSort tail (insert hd acc)
    insertionSort xs []

let rec quickSort xs =
    match xs with
    | [] -> []
    | hd :: tail ->
        let (left, right) = separateBy (fun x -> x < hd) tail
        append (quickSort left) (hd :: (quickSort right))

let bubbleSort xs =
    let rec bubbleSort xs acc n =
        let continueOrReturn acc =
            if n = 0 then reverse acc else bubbleSort (reverse acc) [] (n - 1)

        match xs with
        | [] -> continueOrReturn acc
        | [hd] -> continueOrReturn (hd :: acc)
        | hd :: el :: tail when hd <= el -> bubbleSort (el :: tail) (hd :: acc) n
        | hd :: el :: tail -> bubbleSort (hd :: tail) (el :: acc) n
    
    bubbleSort xs [] (length xs)

let selectionSort xs =
    let rec selectionSort xs acc n =
        match n with
        | 0 -> reverse acc
        | _ ->
            let el = min xs
            selectionSort (excludeOne (fun x -> x = el) xs) (el :: acc) (n - 1)
    selectionSort xs [] (length xs)

let merge xs ys = 
    let rec merge xs ys acc =
        match xs, ys with
        | [], [] -> reverse acc
        | xs, [] -> append (reverse acc) xs
        | [], ys -> append (reverse acc) ys
        | x :: xs, y :: ys when x <= y -> merge xs (y :: ys) (x :: acc)
        | x :: xs, y :: ys -> merge (x :: xs) ys (y :: acc)
    merge xs ys []

let rec mergeSort xs =
    match length xs / 2 with // int division 
    | n when n = 0 -> xs
    | n ->
        let (left, right) = splitAt n xs 
        merge (mergeSort left) (mergeSort right)
