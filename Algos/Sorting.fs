module Sorting

open ListUtils

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
        append left (hd :: (quickSort right))

let bubbleSort xs =
    let rec bubbleSort xs acc n =
        match xs with
        | [] -> if n = 0 then acc else bubbleSort acc [] (n - 1)
        | [hd] -> if n = 0 then hd :: acc else bubbleSort acc [] (n - 1)
        | hd :: el :: tail when hd <= el -> bubbleSort tail (hd :: el :: acc) n
        | hd :: el :: tail -> bubbleSort tail (el :: hd :: acc) n
    bubbleSort xs [] (length xs)
