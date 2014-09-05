namespace Fred

// TODO:
// Short-hand string so you can say /[a-z]+/ or whatever <-- ?? see puffnfresh's comments about "improve regex by removing stringly typing & parsing"
// Counts a la {3} {2,4} {2,}
// Character classes
// Reduction parsers apply a function to a match.
// Negation
// Active patterns for empty/nullable parsers
// Check against the rules in http://plastic-idolatry.com/erik/oslo2014.pdf Investigate application of derivatives to other *-semirings
// Operator overloading? + -> Union, * -> Cat, ... ?
// Pull out the *-semiring interface, and generalise the derivative to work on *-semirings.
// Optimise empty and nullable by using fixpoints and caching, because they're used all over the place
//
// SOUNDS GREAT BUT:
// Unions across more than 2 options:
// * UnionN of list: 3x slower than folded Unions (for 5 elements)
// * UnionN of array: 2x slower than folded Unions (for 5 elements)

module Regex =
    open System.Text // For StringBuilder
    type Parser<'a> when 'a: comparison =     // "when 'a: comparison" because we use a Set to store parse trees.
        | Empty                               // The non-matching parser. The error state.
        | Eps                                 // Match the empty string. Useful in language definition.
        | Eps' of Set<'a list>                // Match the empty string, and contain a partial parse tree. Only appears mid-parse.
        | Char of 'a                          // Match a single character/object
        | Union of Parser<'a> * Parser<'a>    // This or that
        | Cat of Parser<'a> * Parser<'a>      // This _then_ that
        | Star of Parser<'a>                  // Zero or more

    // nullable returns true if the parser has found a complete match: it has recognised a string.
    // Put another way, nullable returns true if the parser will recognise/accept the empty string.
    let rec nullable = function
        | Empty        -> false
        | Eps          -> true
        | Eps' _       -> true
        | Char _       -> false
        | Union (a, b) -> nullable a || nullable b
        | Cat (a, b)   -> nullable a && nullable b
        | Star a       -> true

    // empty returns true if a parser has failed to recognise a string.
    let rec empty = function
        | Empty        -> true
        | Eps          -> false
        | Eps' _       -> false
        | Char _       -> false
        | Union (a, b) -> empty a && empty b
        | Cat (a, b)   -> empty a || empty b
        | Star p       -> false

    // Given two sequences and a function, return the result of applying
    // that function to every pair of values in the sequences.
    // Think of Scheme's for/set.
    let allPairs xs ys f =
        let constantly v _ = v
        let left = xs
        let right = ys
        seq { for x in left do
                for y in right do
                    yield f x y}

    // Use parseNull to retrieve all possible parses of the input thus far.
    let rec parseNull (p: Parser<'a>): Set<'a list> =
        let uniq l = l |> Seq.ofList |> Seq.distinct |> List.ofSeq
        match p with
        | Empty
        | Eps
        | Char _  -> Set.empty
        | Eps' t  -> t
        | Union (a,b) -> Set.union (parseNull a) (parseNull b)
        | Cat (a, b)  -> let prefix = parseNull a
                         let suffix = parseNull b
                         allPairs prefix suffix (fun x y -> List.append x y)
                         |> Seq.map List.ofSeq
                         |> Set.ofSeq
        | Star a -> parseNull a

    // dP returns the derivative of a parser combinator with respect to the input token c.
    // That is, d returns a parser that accepts _the rest of the input except for the prefix token c_.
    // The result of dP contains a _partial parse_.
    let rec dP c = function
    | Empty                      -> Empty
    | Eps                        -> Empty
    | Eps' _                     -> Empty
    | Char x when x = c          -> Eps' (Set.singleton [x])
    | Char _                     -> Empty
    | Union (a, b)               -> Union (dP c a, dP c b)
                                    // This line is the only difference between d and dP.
                                    // d can optimise away the first part of the first
                                    // part of the Union because it doesn't build parse
                                    // trees. Annoying that I don't know how to remove the
                                    // duplication of the rest of the functions!
    | Cat (a, b) when nullable a -> Union (Cat (Eps' (parseNull a), dP c b),
                                           Cat (dP c a, b))
    | Cat (a, b)                 -> Cat (dP c a, b)
    | Star a                     -> Cat (dP c a, Star a)

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

    // generate generates every word in the language described by a parser p.
    // It does so in a fair manner, in that the union of two parsers a and b
    // will return words from a and b, in order.
    let generate p =
        let rec gen = function
        | Empty       -> Seq.empty
        | Eps
        | Eps' _      -> seq { yield [] } // generate doesn't use the parse trees.
        | Char c      -> seq { yield [c] }
        | Union (a,b) -> interleave2 (gen a) (gen b)
        | Cat (a,b)   -> seq {
                              let seqA = gen a
                              let seqB = gen b
                              match Seq.isEmpty seqA, Seq.isEmpty seqB with
                              | true, true
                              | true, false
                              | false, true  -> yield! Seq.empty
                              | false, false -> yield! (allPairs seqA seqB (fun a b -> List.append a b))
                            }
        | Star a      -> seq {
                              yield! gen (Eps' (Set.singleton []))
                              yield! gen a } // <-- This is wrong. See the generate-kleene-star-languages branch for explorations in fixing the bug.
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

    // postfixWalk walks a regex parser in depth first order, running a function f on each
    // parser. postfixWalk applies the merge parameter when exiting a subtree, takes a
    // parent node and a pair of _possibly new_ child nodes.
    // (We pretend that Star has a second child, always Empty, to avoid passing around
    // multiple merge-like functions. The alternative - use a parent + list of children
    // as signature - means throwing away arity protection.)
    let rec postfixWalk f merge p =
        match p with
        | Empty
        | Eps
        | Eps' _
        | Char _      -> f p
        | Cat (a,b)   -> merge p (postfixWalk f merge a) (postfixWalk f merge b)
        | Union (a,b) -> merge p (postfixWalk f merge a) (postfixWalk f merge b)
        | Star a      -> merge p (postfixWalk f merge a) Empty // Stinky hack so that we only need one merge function, taking two children.

    // compact removes from a parser those subtrees that can no longer contribute to constructing parses.
    // We can remove Eps nodes, but not Eps' nodes. The latter contain partial parses, so removing them
    // means losing information.
    // You can only remove Eps subparsers from a Union when both subparsers are Eps, because Union (a, Eps)
    // means "the language defined by a, or the empty string". You _could_ remove the Eps if a was nullable,
    // but nullable is O(n) already, so optimising for smallest parser possible means taking longer.
    let compact p =
        p |>
        postfixWalk (fun x -> x)
                    (fun parent childA childB ->
                                                 match parent with
                                                 | Empty | Eps | Eps' _ | Char _ -> parent
                                                 // TODO: Compact Cat (Eps' a, Eps' b) to something like Eps' (a@b)
                                                 // Easiest done with a Red parser.
                                                 | Cat _ -> match childA,childB with
                                                            | Empty, _     -> Empty
                                                            | _, Empty     -> Empty
                                                            | Eps, Eps     -> Eps
                                                            | Eps, _       -> childB
                                                            | _, Eps       -> childA
                                                            | _            -> Cat (childA, childB)
                                                 | Union _ -> match childA,childB with
                                                              | Empty, Empty -> Empty
                                                              | Empty, _     -> childB
                                                              | _,     Empty -> childA
                                                                                // This also compacts Union (Eps,Eps) -> Eps
                                                              | _            -> if childA = childB then childA else Union (childA, childB)
                                                 | Star _ -> match childA with // Note we ignore childB.
                                                             | Empty | Eps | Eps _ -> Eps
                                                             | _ -> Star childA)

    // d returns the derivative of a parser of a CFL with respect to the input token c.
    // That is, d returns a parser that accepts _the rest of the input except for the prefix token c_.
    let rec d c = function
    | Empty                      -> Empty
    | Eps                        -> Empty
    | Eps' _                     -> Empty
    | Char x when x = c          -> Eps
    | Char _                     -> Empty
    | Union (a, b)               -> Union (d c a, d c b)
    | Cat (a, b) when nullable a -> Union (d c b, Cat (d c a, b))
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
            | Eps' t ->
                sb.AppendLine(sprintf "%A [label=\"Eps' [%A]\"]" nextNodeIdx t) |> ignore
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

    // Helper functions

    // any is like Union, but for n tokens, not just two.
    let rec any tokens =
        match tokens with
        | [] -> Empty
        | [x] -> Char x
        | x::xs -> Union (Char x, any xs)

    let alpha = any (List.append ['A'..'Z'] ['a'..'z'])
    let num = any ['0'..'9']
    let alphanum = Union (alpha, num)

    // alltokens is like Cat, but for n tokens, not just two.
    let rec all tokens =
        match tokens with
        | []    -> Empty
        | [x]   -> Char x
        | x::xs -> Cat (Char x, (allTokens xs))