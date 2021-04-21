module ListSortingAlgorithms.Tests

open NUnit.Framework
open FsUnit
open ListSortingAlgorithms

type TestRow<'a> =
    { name: string
      args: 'a list
      expected: 'a list }

let dataset () =
    [ { name = "Empty List"
        args = []
        expected = [] }
      { name = "List of 1 element"
        args = [ 1 ]
        expected = [ 1 ] }
      { name = "Sorted List"
        args = [ 1; 2; 3; 4; 5 ]
        expected = [ 1; 2; 3; 4; 5 ] }
      { name = "Sorted in descending order"
        args = [ 5; 4; 3; 2; 1 ]
        expected = [ 1; 2; 3; 4; 5 ] }
      { name = " Unsorted list of 10 numbers"
        args = [ 5; 7; 3; 2; 1; 8; 9; 0; 14; 17 ]
        expected = [ 0; 1; 2; 3; 5; 7; 8; 9; 14; 17 ] } ]



[<Test>]
[<TestCaseSource("dataset")>]
let TestQuickSort (row) =

    quickSort row.args |> should equal row.expected

[<Test>]
[<TestCaseSource("dataset")>]
let TestMergeSort (row) =

    mergeSort row.args |> should equal row.expected

[<Test>]
[<TestCaseSource("dataset")>]
let TestSelectionSort (row) =

    selectionSort row.args
    |> should equal row.expected


[<Test>]
[<TestCaseSource("dataset")>]
let TestHeapSort (row) =
    heapSort row.args |> should equal row.expected
