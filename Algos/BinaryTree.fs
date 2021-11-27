module Algos.BinaryTree

type BinaryTree<'T> =
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>
    | Leaf

type TraversalMode =
    | PreOrder
    | InOrder
    | PostOrder

let preOrderTraversal tree =
    let rec preOrderTraversal tree acc =
        match tree with
        | Leaf -> acc
        | Node (data, left, right) ->
            let traversedRight = preOrderTraversal right acc // traverse right first (as it will appended to the end)
            data :: preOrderTraversal left traversedRight // traverse left and cons root

    preOrderTraversal tree []

let inOrderTraversal tree =
    let rec inOrderTraversal tree acc =
        match tree with
        | Leaf -> acc
        | Node (data, left, right) ->
            let traversedLeft = data :: (inOrderTraversal left acc) // traverse left and append data
            inOrderTraversal right traversedLeft // traverse right
    
    List.rev (inOrderTraversal tree [])

let postOrderTraversal tree =
    let rec postOrderTraversal tree acc =
        match tree with
        | Leaf -> acc
        | Node (data, left, right) ->
            let traversedLeft = postOrderTraversal left acc // go to left most node first
            data :: postOrderTraversal right traversedLeft // go to left most node of the right node and append root
    
    List.rev (postOrderTraversal tree [])

let depthTraversal tree traversalMode =
    match traversalMode with
    | PreOrder -> preOrderTraversal tree
    | InOrder -> inOrderTraversal tree
    | PostOrder -> postOrderTraversal tree