namespace Fred

// TODO:
// Short-hand string so you can say /[a-z]+/ or whatever
// Counts a la {3} {2,4} {2,}
// Character classes
// Unions across more than 2 options
module Regex =
    open System.Text // For StringBuilder
    type Parser<'a> =
        | Empty                               // The non-matching parser. The error state.
        | Eps                                 // Match the empty string.
        | Char of 'a                          // Match a single character/object
        | Union of Parser<'a> * Parser<'a>    // This or that
        | Cat of Parser<'a> * Parser<'a>      // This _then_ that
        | Star of Parser<'a>                  // Zero or more

    // nullable returns true if the parser has found a complete match: it has recognised a string.
    // Put another way, nullable returns true if the parser will recognise/accept the empty string.
    let rec nullable = function
        | Empty        -> false
        | Eps          -> true
        | Char _       -> false
        | Union (a, b) -> nullable a || nullable b
        | Cat (a, b)   -> nullable a && nullable b
        | Star a       -> true

    // d returns the derivative of a parser with respect to the input token c.
    // That is, d returns a parser that accepts _the rest of the input except for the prefix token c_.
    let rec d c = function
        | Empty                      -> Empty
        | Eps                        -> Empty
        | Char x when x = c          -> Eps
        | Char _                     -> Empty
        | Union (a, b)               -> Union (d c a, d c b)
        | Cat (a, b) when nullable a -> Union (d c b, Cat (d c a, d c b))
        | Cat (a, b)                 -> Cat (d c a, b)
        | Star a                     -> Cat (d c a, Star a)

    // matches returns true if a parser matches the entire input.
    let rec matches p = function
        | []    -> nullable p
        | x::xs -> matches (d x p) xs

    // matchSeq returns true if a parser matches the entire input seq. Useful for matching Strings.
    let matchSeq p s = 
        List.ofSeq s
        |> matches p

    // empty returns true if a parser has failed to recognise a string.
    let rec empty = function
        | Empty        -> true
        | Eps          -> false
        | Char _       -> false
        | Union (a, b) -> empty a && empty b
        | Cat (a, b)   -> empty a || empty b
        | Star p       -> false

    // greedy returns true when a parser tries to match the longest possible sequence of input
    let rec greedy = function
        | Empty        -> false
        | Eps          -> false
        | Char _       -> false
        | Union (a, b) -> greedy a || greedy b
        | Cat (a, b)   -> greedy a || greedy b
        | Star _       -> true

    // compact will return a new parser that recognises the same language, but is structurally simpler.
    let rec compact p =
        let makeCompactParser test baseParser ctor compactA compactB =
            match test compactA, test compactB with
            | true, true -> baseParser
            | true, false -> compactB
            | false, true -> compactA
            | false, false -> ctor (compactA, compactB)
        let compactCat a b =
            let a' = compact a
            let b' = compact b
            makeCompactParser nullable Eps (fun (x, y) ->
                makeCompactParser empty Empty Cat a' b') a' b'
        match p with
        | p when empty p              -> Empty
        | Empty                       -> Empty // Technically, this is redundant: the first clause will take care of Empty parsers
        | Eps                         -> Eps
        | Char c                      -> Char c
        | Union (a,b) -> let a' = compact a
                         let b' = compact b
                         if a' = b' then a'
                         else makeCompactParser empty Empty Union a' b'
        | Cat (a, b) -> compactCat a b
        | Star a when empty a         -> Eps // Kleene star can always accept the empty string!
        | Star a                      -> Star (compact a)

    // interleave returns a sequence that draws elements from each of the sequences in turn.
    // As each sequence empties, interleave forgets about the sequence.
    // For instance interleave [Seq.ofList [1;2;3]; Seq.ofList [4;5;6]; Seq.empty; Seq.ofList [10;11]
    // returns, in order, [1;4;10;2;5;11;3;6].
    let rec interleave = function
        | [] -> Seq.empty
        | fst::rest -> seq {
                            if Seq.isEmpty fst then
                                yield! interleave rest
                            else
                                yield Seq.head fst
                                // Rotate the list of sequences, so we round-robin.
                                yield! interleave (List.append rest [Seq.skip 1 fst]) }
    let interleave2 a b = interleave [a;b]

    let rec generate = function
        | Empty -> Seq.empty
        | Eps -> seq { yield [] }
        | Char c -> seq { yield [c] }
        | Union (a,b) -> interleave2 (generate a) (generate b)
        | Cat (a,b) -> seq {
                            let a = Seq.head (generate a)
                            let b = Seq.head (generate b)
                            yield List.append a b }
        | Star a -> seq {
            yield! generate Eps
            yield! generate a }

    // findSubMatch finds the prefix of some input that matches a parser. It returns
    // the matching prefix, and the remainder of the input.
    // Current Star parsers don't return the correct results because they match _parsimoniously_.
    // They need to match _greedily_.
    let findSubMatch p input =
        let rec find subParser partialMatch xs =
            printfn "find %A partial: %A input: %A (empty? %A nullable? %A)" subParser partialMatch xs (empty subParser) (nullable subParser)
            match subParser, xs with
            | _, [] when empty subParser -> [], []
            | _, [] when nullable subParser -> partialMatch, [] // New match, no more input
            | _, [] -> [], []
            | _, x::rest when empty subParser -> find p [] (List.tail (List.append partialMatch xs)) // No match, move along input _from the original start point_
            | _, x::rest when greedy subParser -> find (d x subParser) (x::partialMatch) rest
            | _, x::rest when nullable subParser -> partialMatch, xs // New match, move along
            | _, x::rest -> let newMatches, dregs = find (d x subParser) (x::partialMatch) rest
                            newMatches, dregs
        find p [] input

    // findMatches returns a list of _all_ matches that a parser finds in the input.
    let findMatches p s =
        let toString chars =
            List.fold (fun (sb: StringBuilder) (c: char) -> sb.Append(c)) (new StringBuilder()) chars
            |> string
        let mutable remainder = (List.ofSeq s)
        let mutable matches = []
        while not (List.isEmpty remainder) do
            let newMatch, rest = findSubMatch p remainder
            remainder <- rest
            matches   <- match newMatch with
                         | []    -> matches
                         | x::xs -> newMatch::matches
        List.map (fun x -> toString (List.rev x)) (List.rev matches)

    // dotify turns a Regex parser into a string in graphviz's DOT format.
    let dotify p =
        // We deliberately entangle walking the parser tree with printing
        // just because it's _shorter_. Untangling and removing duplication
        // takes up about twice as much screen estate, primarily because of
        // all the extra pattern matching (one per thing).
        // dotify' returns the highest node index in the parser (thus far)
        let rec dotify' p (sb: StringBuilder) nextNodeIdx =
            match p with
            | Empty ->
                sb.AppendLine(sprintf "%A [label=\"Empty\"]" nextNodeIdx) |> ignore
                nextNodeIdx + 1
            | Eps ->
                sb.AppendLine(sprintf "%A [label=\"Eps\"]" nextNodeIdx) |> ignore
                nextNodeIdx + 1
            | Char c ->
                sb.AppendLine(sprintf "%A [label=\"Char %A\"]" nextNodeIdx c) |> ignore
                nextNodeIdx + 1
            | Union (a, b) ->
                sb.AppendLine(sprintf "%A [label=\"Union\"]" nextNodeIdx) |> ignore
                sb.AppendLine(sprintf "%A -> %A" nextNodeIdx (nextNodeIdx + 1)) |> ignore
                let highest = (dotify' a sb (nextNodeIdx + 1))
                sb.AppendLine(sprintf "%A -> %A" nextNodeIdx highest) |> ignore
                (dotify' b sb highest)
            | Cat (a, b) ->
                sb.AppendLine(sprintf "%A [label=\"Cat\"]" nextNodeIdx) |> ignore
                sb.AppendLine(sprintf "%A -> %A" nextNodeIdx (nextNodeIdx + 1)) |> ignore
                let highest = (dotify' a sb (nextNodeIdx + 1))
                sb.AppendLine(sprintf "%A -> %A" nextNodeIdx highest) |> ignore
                (dotify' b sb highest)
            | Star a ->
                sb.AppendLine(sprintf "%A [label=\"Star\"]" nextNodeIdx) |> ignore
                sb.AppendLine(sprintf "%A -> %A" nextNodeIdx (nextNodeIdx + 1)) |> ignore
                dotify' a sb (nextNodeIdx + 1)
        let sb = new StringBuilder()
        sb.AppendLine("digraph {") |> ignore
        dotify' p sb 0 |> ignore
        sb.AppendLine("}")
        |> string