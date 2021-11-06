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
    let rec separateBy f xs left right = 
        match xs with
        | [] -> (left, right)
        | hd :: tail when f hd -> separateBy f tail (hd :: left) right
        | hd :: tail -> separateBy f tail left (hd :: right)
    separateBy f xs [] []
