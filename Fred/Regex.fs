namespace Fred

// TODO:
// Short-hand string so you can say /[a-z]+/ or whatever
// Counts a la {3} {2,4} {2,}
// Character classes
// Unions across more than 2 options
// Move partial matches into the Eps parser, like in Might et al's paper
// Active patterns for empty/nullable parsers
// Zipper-based compaction, that will compact from the leaves, working its
// way to the root by compacting in the "make new node" step.
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

    // empty returns true if a parser has failed to recognise a string.
    let rec empty = function
        | Empty        -> true
        | Eps          -> false
        | Char _       -> false
        | Union (a, b) -> empty a && empty b
        | Cat (a, b)   -> empty a || empty b
        | Star p       -> false

    // interleave returns a sequence that draws elements from each of the sequences in turn.
    // As each sequence empties, interleave forgets about the sequence.
    // For instance interleave [Seq.ofList [1;2;3]; Seq.ofList [4;5;6]; Seq.empty; Seq.ofList [10;11]
    // returns, in order, [1;4;10;2;5;11;3;6].
    // O(n^2)! Likely because of that List.append...
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

    // interleaveSeq returns a sequence that draws elements from each of the sequences in turn.
    // As each sequence empties, interleave forgets about the sequence.
    // O(n), but doesn't interleave fairly!
    let rec interleaveSeq seqs =
        seq {
             for s in seqs do
                 if not (Seq.isEmpty s) then
                     yield Seq.head s
             let remainder = Seq.filter (fun s -> not (Seq.isEmpty s)) seqs
             if not (Seq.isEmpty remainder) then
                 yield! interleaveSeq (Seq.map (Seq.skip 1) remainder)}

    // prod turns a pair of Seqs into a Seq of pairs representing
    // the Cartesian product of the Seqs.
    let rec prod xs ys =
        seq {
            for x in xs do
                for y in ys do
                    yield x,y}

    // generate generates every word in the language described by a parser p.
    // It does so in a fair manner, in that the union of two parsers a and b
    // will return words from a and b, in order.
    let generate p =
        let rec gen = function
        | Empty       -> Seq.empty
        | Eps         -> seq { yield [] }
        | Char c      -> seq { yield [c] }
        | Union (a,b) -> interleave2 (gen a) (gen b)
        | Cat (a,b)   -> seq {
                              let seqA = gen a
                              let seqB = gen b
                              match Seq.isEmpty seqA, Seq.isEmpty seqB with
                              | true, true
                              | true, false
                              | false, true  -> yield! Seq.empty
                              | false, false -> yield! (prod seqA seqB |> Seq.map (fun (a,b) -> List.append a b))
                            }
        | Star a      -> seq {
                              yield! gen Eps
                              yield! gen a }
        Seq.distinct (gen p)

    // exactlyEqual returns true if the only value that sequence yields - and only once - is value.
    // It's like value = Seq.exactlyOne seq, only doesn't throw an exception.
    let exactlyEqual sequence value =
        match Seq.isEmpty sequence with
            | true  -> false
            | false ->
                let v = Seq.head sequence
                let s' = Seq.skip 1 sequence
                match Seq.isEmpty s' with
                | false -> false
                | true  -> v = value

    let rec postfixWalk f (merge: Parser<'a> -> Parser<'a> -> Parser<'a> -> Parser<'a>) p =
        match p with
        | Empty
        | Eps
        | Char _      -> f p
        | Cat (a,b)   -> merge p (postfixWalk f merge a) (postfixWalk f merge b)
        | Union (a,b) -> merge p (postfixWalk f merge a) (postfixWalk f merge b)
        | Star a      -> merge p (postfixWalk f merge a) Empty // Stinky hack so that we only need one merge function, taking two children.

    let compact p =
        p |>
        postfixWalk (fun x -> x)
                    (fun parent childA childB -> //printfn "%A <- %A + %A" parent childA childB
                                                 match parent with
                                                 | Empty | Eps | Char _ -> parent
                                                 | Cat _ -> match childA,childB with
                                                            | Empty, _ -> Empty
                                                            | _, Empty -> Empty
                                                            | Eps, Eps -> Eps
                                                            | Eps, _   -> childB
                                                            | _,   Eps -> childA
                                                            | _        -> Cat (childA, childB)
                                                 | Union _ -> match childA,childB with
                                                              | Empty, Empty -> Empty
                                                              | Empty, _     -> childB
                                                              | _,     Empty -> childA
                                                              | _            -> if childA = childB then childA else Union (childA, childB)
                                                 | Star _ -> match childA with // Note we ignore childB.
                                                             | Empty | Eps -> Eps
                                                             | _ -> Star childA)

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
        | x::xs ->
            let deriv = (d x p)
            let c = deriv//compact deriv
//            printfn "d c p = %A" deriv
//            printfn "compact = %A" c
            matches c xs

    // matchSeq returns true if a parser matches the entire input seq. Useful for matching Strings.
    let matchSeq p s =
        List.ofSeq s
        |> matches p

    // findSubMatch finds the prefix of some input that matches a parser. It returns
    // the matching prefix, and the remainder of the input.
    // Current Star parsers don't return the correct results because they match _parsimoniously_.
    // They need to match _greedily_.
    let findSubMatch p input =
        let rec find subParser partialMatch xs =
//            printfn "find %A partial: %A input: %A (empty? %A nullable? %A)" subParser partialMatch xs (empty subParser) (nullable subParser)
            match subParser, xs with
            | Star rep, _ ->
                let mutable remainder = xs
                let mutable entireMatch = partialMatch
                let mutable finished = false
                while not finished do
                    let newMatch, dregs = find rep [] remainder
                    if not (List.isEmpty newMatch) then
                        // No match: end of the line for this match attempt
                        remainder <- dregs
                        entireMatch <- List.append entireMatch newMatch
                    finished <- (List.isEmpty newMatch) || (List.isEmpty remainder)
                entireMatch, remainder
            | _, _ when nullable subParser -> partialMatch, xs // New match
            | _, _ when empty subParser -> [], xs
            | _, [] -> [], []
            | _, x::rest -> let newMatches, dregs = find (d x subParser) (x::partialMatch) rest
                            newMatches, dregs
        // If we found no match, return the actual remaining input... which is the remaining input.
        match find p [] input with
        | [], r -> [], input
        | m, r -> m, r

    // findMatches returns a list of _all_ matches that a parser finds in the input.
    // It attempts to find a prefix match - a match on the first items in the input - and, if
    // that fails, moves one item along the input. Crude, inefficient, but it works.
    let findMatches p s =
        let toString chars =
            List.fold (fun (sb: StringBuilder) (c: char) -> sb.Append(c)) (new StringBuilder()) chars
            |> string
        let mutable remainder = (List.ofSeq s)
        let mutable matches = []
        while not (List.isEmpty remainder) do
            let newMatch, rest = findSubMatch p remainder
            remainder <- match newMatch with
                         | [] -> List.tail remainder // No match? Move one item along.
                         | _  -> rest
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