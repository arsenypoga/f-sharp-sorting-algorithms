// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open ListSortingAlgorithms

type TestRow<'a> =
    { name: string
      got: 'a list
      expected: 'a list }

let dataset =
    [ { name = "Empty List"
        got = []
        expected = [] }
      { name = "List of 1 element"
        got = [ 1 ]
        expected = [ 1 ] }
      { name = "Sorted List"
        got = [ 1; 2; 3; 4; 5 ]
        expected = [ 1; 2; 3; 4; 5 ] }
      { name = "Sorted in descending order"
        got = [ 5; 4; 3; 2; 1 ]
        expected = [ 1; 2; 3; 4; 5 ] }
      { name = " Unsorted list of 10 numbers"
        got = [ 5; 7; 3; 2; 1; 8; 9; 0; 14; 17 ]
        expected = [ 0; 1; 2; 3; 5; 7; 8; 9; 14; 17 ] } ]



[<EntryPoint>]
let main argv =
    for item in dataset do
        printfn "heapSort:  expected: %A  got: %A" item.expected (heapSort item.got)

    0
