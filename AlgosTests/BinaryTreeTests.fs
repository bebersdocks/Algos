module AlgosTests.BinaryTreeTests

open System

open Algos.BinaryTree

open AlgosTests.TestUtils

open Xunit

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

[<Fact>]
let ``Pre-order traversal`` () =
    haveSameContent (depthTraversal characterTree PreOrder) ['F'; 'B'; 'A'; 'D'; 'C'; 'E'; 'G'; 'I'; 'H']

[<Fact>]
let ``In-order traversal`` () =
    haveSameContent (depthTraversal characterTree InOrder) ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I']

[<Fact>]
let ``Post-order traversal`` () =
    haveSameContent (depthTraversal characterTree PostOrder) ['A'; 'C'; 'E'; 'D'; 'B'; 'H'; 'I'; 'G'; 'F']
