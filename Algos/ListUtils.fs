module ListUtils

let length xs =
    let rec length xs n =
        match xs with 
        | [] -> n
        | _ :: tail -> length tail (n + 1)
    length xs 0

/// <summary>
/// Separates list in two by predicate.
/// </summary>
/// <returns>Tuple of two lists.</returns>
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

exception ListUtilsException of string

let head = function
    | [] -> raise (ListUtilsException "empty list doesn't have head")
    | hd :: _ -> hd

let min xs =
    let rec min xs current =
        match xs with
        | [] -> current
        | hd :: tail when hd < current -> min tail hd 
        | _ :: tail -> min tail current

    min xs (head xs)

let excludeOne f xs =
    let rec excludeOne xs acc =
        match xs with
        | [] -> acc
        | hd :: tail when f hd -> append acc tail 
        | hd :: tail -> excludeOne tail (hd :: acc)
    excludeOne xs []
