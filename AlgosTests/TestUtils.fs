module AlgosTests.TestUtils

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
