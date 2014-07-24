namespace Fred

// TODO:
// Drawing parsers/grammars
// Short-hand string so you can say /[a-z]+/ or whatever
module Regex =
    open System.Text
    type Parser<'a> =
        | Empty                               // The non-matching parser. The error state.
        | Eps                                 // Match the empty string.
        | Char of 'a                          // Match a single character/object
        | Union of Parser<'a> * Parser<'a>    // This or that
        | Cat of Parser<'a> * Parser<'a>      // This _then_ that
        | Star of Parser<'a>                  // Zero or more

    // nullable returns true if the parser has found a complete match: it has recognised a string.
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

    // findMatches returns a list of _all_ matches that a parser finds in the input.
    // Current Star parsers cause an infinite loop because they match _parsimoniously_,
    // and don't consume any input. They need to match _greedily_.
    let findMatches p s =
        let toString chars =
            List.fold (fun (sb: StringBuilder) (c: char) -> sb.Append(c)) (new StringBuilder()) chars
            |> string
        let rec find subParser partialMatch matches xs =
            printfn "find %A partial: %A matches: %A input: %A (empty? %A nullable? %A)" subParser partialMatch matches xs (empty subParser) (nullable subParser)
            match subParser, xs with
            | _, [] when empty subParser -> matches, []
            | _, [] when nullable subParser -> partialMatch::matches, [] // New match, no more input
            | _, [] -> matches, []
            | Star p, x::rest -> find (d x subParser) (x::partialMatch) matches rest
            | _, x::rest when empty subParser -> find p [] matches (List.tail (List.append partialMatch xs)) // No match, move along input _from the original start point_
            | _, x::rest when nullable subParser -> partialMatch::matches, xs // New match, move along
            | _, x::rest -> let newMatches, dregs = find (d x subParser) (x::partialMatch) matches rest
                            List.append newMatches matches, dregs
        let mutable remainder = (List.ofSeq s)
        let mutable matches = []
        while not (List.isEmpty remainder) do
            let newMatches, rest = find p [] [] remainder
            remainder <- rest
            matches <- List.append newMatches matches
        List.map (fun x -> toString (List.rev x)) (List.rev matches)

    let rec walk f p =
        match p with
        | Empty -> [f p]
        | Eps -> [f p]
        | Char _ -> [f p]
        | Union (a, b) -> (f p)::(List.append (walk f a) (walk f b))
        | Cat (a, b) -> (f p)::(List.append (walk f a) (walk f b))
        | Star a -> (f p)::(walk f a)

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