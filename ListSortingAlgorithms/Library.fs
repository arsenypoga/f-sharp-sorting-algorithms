module ListSortingAlgorithms

// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


//  bubblesort,  heapsort,  insertionsort,  mergesort,  quicksort,  radixsort  and  selectionsort.

(*
    Swap item indexes
    *)

let rec selectionSort =
    function
    | [] -> []
    | item :: head -> // Cons pattern
        let smallest, rest =
            List.fold
                (fun (smallest, acc) x ->
                    if x < smallest then
                        (x, smallest :: acc) // smallest prepend acc
                    else
                        (smallest, x :: acc)) // x prepend acc
                (item, [])
                head

        smallest :: selectionSort rest

let rec bubbleSort =
    function
    | [] -> []
    | a -> a
