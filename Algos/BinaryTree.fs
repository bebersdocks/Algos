module BinaryTree

type BinaryTree<'T> =
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>
    | Leaf

type TraversalMode =
    | InOrder
    | PreOrder
    | PostOrder

let traversalModeToString = function
    | InOrder -> "InOrder"
    | PreOrder -> "PreOrder"
    | PostOrder -> "PostOrder"

let inOrderTraversal tree =
    let rec inOrderTraversal tree acc = 
        match tree with
        | Leaf -> acc
        | Node (data, left, right) ->
            let traversedLeft = data :: (inOrderTraversal left acc) // traverse left and append data
            inOrderTraversal right traversedLeft // traverse right
    
    List.rev (inOrderTraversal tree [])

let preOrderTraversal tree =
    let rec preOrderTraversal tree acc = 
        match tree with
        | Leaf -> acc
        | Node (data, left, right) ->
            let traversedRight = preOrderTraversal right acc // traverse right first (as it will appended to the end)
            data :: preOrderTraversal left traversedRight // traverse left and cons root

    preOrderTraversal tree []

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
    | InOrder -> inOrderTraversal tree
    | PreOrder -> preOrderTraversal tree
    | PostOrder -> postOrderTraversal tree

let characterTree = 
    Node ('F',
        Node ('B',
            Node ('A', Leaf, Leaf),
            Node ('D', 
                Node ('C', Leaf, Leaf),
                Node ('E', Leaf, Leaf))),
        Node ('G', 
            Leaf,
            Node ('I', 
                Node ('H', Leaf, Leaf), 
                Leaf)))

// TODO: move to tests
printfn "%A" (depthTraversal characterTree InOrder)
printfn "%A" (depthTraversal characterTree PreOrder)
printfn "%A" (depthTraversal characterTree PostOrder)
