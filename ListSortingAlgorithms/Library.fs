module ListSortingAlgorithms

// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


//  bubblesort,  heapsort,  insertionsort,  ,  ,  radixsort  and  .

// SelectionSort


let rec selectionSort =
    function
    | [] -> []
    | [ _ ] as list -> list
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

// Given a tuple of 2 sorted lists ([], []), combine them to form a single sorted list ([]).
let rec private _mergeParts =
    function
    // If either list is empty, return the other
    | ([], list)
    | (list, []) -> list
    // If the head of list1 is greater than the head of list2, pop it off and keep sorting recursively
    | (head :: rest, other) when head <= other.Head -> [ head ] @ (_mergeParts (rest, other))
    // If the head of list2 is greater than the head of list1, pop it off and keep sorting recursively
    | (other, head :: rest) when head <= other.Head -> [ head ] @ (_mergeParts (rest, other))

    // If we get passed something unexpected, just return nothing (I guess we could throw up...)
    | _ -> []

// Given a list, sort it.
let rec mergeSort =
    function
    // If the list has less than 2 elements, just return it as-is (base case)
    | []
    | [ _ ] as list -> list

    // If there are only two elements left, return them sorted
    | [ first; last ] when first <= last -> [ first; last ]
    | [ first; last ] when first >= last -> [ last; first ]

    // Otherwise, recursively mergesort by splitting the array in two and sorting each half
    // and then combine the results using _mergeParts
    | list ->
        list
        |> List.splitAt (list.Length / 2)
        |> fun (head, tail) -> _mergeParts ((mergeSort head), (mergeSort tail))

let rec quickSort =
    function
    // If the list has less than 2 elements, just return it as-is (base case)
    | []
    | [ _ ] as list -> list
    // Otherwise, use the head as a pivot and return sorted values under the pivot,
    // the pivot, and then the sorted values over the pivot.
    | pivot :: list ->
        // This filters down to values smaller than the pivot and then recursively sorts them
        (quickSort (List.filter ((>=) pivot) list))
        @ [ pivot ]
          @
          // This filters down to values larger than the pivot and then recursively sorts them
          (quickSort (List.filter ((<=) pivot) list))


type Node =
    { value: int
      left: Node option
      right: Node option }

let rec insertNode node value =
    match node with
    | Some t when value < t.value ->
        { t with
              left = Some(insertNode t.left value) }
    | Some t when value > t.value ->
        { t with
              right = Some(insertNode t.right value) }
    | Some t -> t
    | None ->
        { value = value
          left = None
          right = None }

let intoHeap list =
    list
    |> List.fold (fun acc item -> Some(insertNode acc item)) None

let rec fromHeap =
    function
    | Some t ->
        (fromHeap t.left)
        @ [ t.value ] @ (fromHeap t.right)
    | None -> []

let heapSort =
    function
    | [] -> []
    | [ _ ] as list -> list
    | a -> fromHeap (intoHeap a)
