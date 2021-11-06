module ListUtils

let length xs =
    let rec length xs n =
        match xs with 
        | [] -> n
        | _ :: tail -> length tail (n + 1)
    length xs 0

///<summary>
/// Separates list in two by predicate.
///</summary>
///<return>Tuple of two lists.</return>
let separateBy f xs =
    let rec separateBy xs left right = 
        match xs with
        | [] -> (left, right)
        | hd :: tail when f hd -> separateBy tail (hd :: left) right
        | hd :: tail -> separateBy tail left (hd :: right)
    separateBy xs [] []

let reverse xs =
    let rec reverse xs acc =
        match xs with 
        | [] -> acc
        | hd :: tail -> reverse tail (hd :: acc)
    reverse xs []

let append xs ys =
    let rec append xs acc = 
        match xs with
        | [] -> acc
        | hd :: tail -> append tail (hd :: acc)
    append (reverse xs) ys
