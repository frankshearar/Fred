namespace Fred

// TODO:
// Add a "reactive" API, so that as you process a stream of things you can Observe the matches.
// Short-hand string so you can say /[a-z]+/ or whatever <-- ?? see puffnfresh's comments about "improve regex by removing stringly typing & parsing"
// Reduction parsers apply a function to a match.
// Wildcard - a parser that accepts any single token. Looks a lot like Char, turns into an Eps/Eps' on derivation.
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

// Bugs in the paper?
// section 5.2.1:
//
//     If e is (Sym c), the automaton has one state, which accepts symbol c; it is not
//     bypassable. Its identifier is n; the next available identifier is n+1. The state’s moves
//     go to the destination states ds.
//     F(e) = {state n}
//     b(e) = true
//     r2n’ (Sym c) n ds = ([State n c ds], n+1, False)
//
// But then surely b(e) = false ?!

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
        | Star _       -> true

    // empty returns true if a parser has failed to recognise a string.
    let rec empty = function
        | Empty        -> true
        | Eps          -> false
        | Eps' _       -> false
        | Char _       -> false
        | Union (a, b) -> empty a && empty b
        | Cat (a, b)   -> empty a || empty b
        | Star _       -> false

    // Given two sequences and a function, return the result of applying
    // that function to every pair of values in the sequences.
    // Think of Scheme's for/set.
    let allPairs xs ys f =
        match Seq.isEmpty xs, Seq.isEmpty ys with
        | true, true -> Seq.empty
        | true, false -> ys
        | false, true -> xs
        | false, false -> seq {
                               for x in xs do
                                   for y in ys do
                                       yield f x y}

    // Use parseNull to retrieve all possible parses of the input thus far.
    let rec parseNull (p: Parser<'a>): Set<'a list> =
        match p with
        | Empty
        | Eps
        | Char _  -> Set.empty
        | Eps' t  -> t
        | Union (a,b) -> Set.union (parseNull a) (parseNull b)
        | Cat (a, b)  -> let prefix = parseNull a
                         let suffix = parseNull b
                         allPairs prefix suffix List.append
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

    let (|LT|EQ|GT|) (a, b) =
        match compare a b with
        | 0 -> EQ
        | n when n < 0 -> LT
        | _ -> GT

    // |/ merges two ordered (according to the elements' compare) sequences
    // such that resulting sequence is ordered (by the elements' compare).
    // (I'd use (\/) like the original paper (McIlroy, Enumerating the Strings
    // of Regular Languages), but no \s allowed!
    let rec (|/) xs ys =
         match Seq.isEmpty xs, Seq.isEmpty ys with
         | true, true -> Seq.empty
         | true, false -> ys
         | false, true -> xs
         | false, false ->
            // Pulling out the tails in let bindings looks neater,
            // but means we throw one of those tails away every
            // time the comparison of heads is not EQ. |/ is used
            // A LOT, so let's be efficient with some loss of beauty.
            let x = Seq.head xs
            let y = Seq.head ys
            match x, y with
            | LT -> seq { yield x; yield! (Seq.skip 1 xs) |/ ys              }
            | EQ -> seq { yield x; yield! (Seq.skip 1 xs) |/ (Seq.skip 1 ys) }
            | GT -> seq { yield y; yield!              xs |/ (Seq.skip 1 ys) }

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

    // map walks a regex parser in depth first order, running a function f on each
    // parser.
    let rec map f p =
        printfn "walking %A" p
        match p with
        | Empty
        | Eps
        | Eps' _
        | Char _      -> f p
        | Cat (a,b)   -> f <| Cat (map f a, map f b)
        | Union (a,b) -> f <| Union (map f a, map f b)
        | Star a      -> f <| Star (map f a)

    // compact removes from a parser those subtrees that can no longer contribute to constructing parses.
    // We can remove Eps nodes, but not Eps' nodes. The latter contain partial parses, so removing them
    // means losing information.
    // You can only remove Eps subparsers from a Union when both subparsers are Eps, because Union (a, Eps)
    // means "the language defined by a, or the empty string". You _could_ remove the Eps if a was nullable,
    // but nullable is O(n) already, so optimising for the smallest parser possible means taking longer.
    let compact p =
        p
        |> map (fun x ->
                     match x with
                     | Empty | Eps | Eps' _ | Char _ -> x
                     // TODO: Compact Cat (Eps' a, Eps' b) to something like Eps' (a@b)
                     // Easiest done with a Red parser.
                     | Cat (Empty,_)
                     | Cat (_, Empty)         -> Empty
                     | Cat (Eps,Eps)          -> Eps
                     | Cat (Eps, b)           -> b
                     | Cat (a, Eps)           -> a
                     | Cat _                  -> x
                     | Union (Empty,Empty)    -> Empty
                     | Union (Empty,b)        -> b
                     | Union (a, Empty)       -> a
                     | Union (a,b) when a = b -> a // This also compacts Union (Eps,Eps) -> Eps
                     | Union _                -> x
                     | Star Empty
                     | Star Eps
                     | Star (Eps' _)          -> Eps
                     | Star _                 -> x)

    // d returns the derivative of a parser of a regular language with respect to the input token c.
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

    type Ident = int
    type State<'a> = {Ident: Ident; Token: 'a}
    type NFA<'a when 'a: comparison> = {Edges: Map<State<'a>,Ident seq>; Starts: Ident list; Finals: Ident list}

    let emptyNFA = Map.empty
    // Which states in nfa have edges to destStateIdx?
    let destStateIdxs (destStateIdx: Ident) nfa: Ident seq =
        nfa.Edges
        |> Map.toSeq
        |> Seq.filter (fun (_,v) -> v |> Seq.exists (fun i -> i = destStateIdx))
        |> Seq.map (fun edge -> (fst edge).Ident)

    let emptyNfa = {Edges = Map.empty ; Starts = []; Finals = []}

    let add (startState: State<'a>) (destState: Ident option) (nfa: NFA<'a>): NFA<'a> = // Add start state?
        let maybeKey = Map.tryFindKey (fun k _ -> k = startState) nfa.Edges
        match maybeKey with
        | Some _ ->
            // We've seen this start-of-edge before
            match destState with
            | Some d ->
                let oldDest = Map.find startState nfa.Edges
                let newDest = seq { yield! oldDest; yield d} |> Seq.distinct
                {nfa with Edges = nfa.Edges |> Map.add startState newDest}
            | None -> nfa
        | None ->
            // New start-of-edge
            match destState with
            | Some d -> {nfa with Edges = nfa.Edges |> Map.add startState (seq {yield d})}
            | None   -> {nfa with Edges = nfa.Edges |> Map.add startState Seq.empty}

    let merge nfa1 nfa2 =
        {Edges = nfa1.Edges |> Map.toList |> List.fold (fun nfa (k,v) -> Map.add k v nfa) nfa2.Edges
         Starts = List.append nfa1.Starts nfa2.Starts
         Finals = List.append nfa1.Finals nfa2.Finals}

    let addStartState ident nfa =
        {nfa with Starts = ident :: nfa.Starts}

    let draw nfa =
        let draw' nfa (sb: StringBuilder) =
            let edges = nfa.Edges |> Map.toList
            edges
            |> Seq.iter (fun (k,v) -> sb.AppendLine(sprintf "    %d [label=\"%A\"]" k.Ident k.Token) |> ignore)
            edges
            |> Seq.iter (fun (src,dests) ->
                dests
                |> Seq.iter (fun x -> sb.AppendLine(sprintf "    %d -> %d" src.Ident x) |> ignore))
        let sb = new StringBuilder()
        sb.AppendLine("digraph {") |> ignore
        draw' nfa sb |> ignore
        sb.AppendLine("}")
        |> string

    let at (index: Ident) (nfa: NFA<'a>): State<'a> =
        nfa.Edges |> Map.tryFindKey (fun k _ -> k.Ident = index) |> Option.get
    let allStates nfa =
        nfa.Edges
        |> Map.toList
        |> List.map fst
    let start nfa =
        allStates nfa
        |> List.maxBy (fun state -> state.Ident)
    let edgesOf (index: Ident) (nfa: NFA<'a>) =
        match nfa.Edges |> Map.tryFindKey (fun k _ -> k.Ident = index) with
        | Some key -> nfa.Edges |> Map.find key
        | None -> Seq.empty

    type R2nResult<'a when 'a: comparison> = {Machine: NFA<'a>; NextIdent: Ident; Bypassable: bool}
    let rec r2n' (p: Parser<'a>) (i: Ident) (destinationStates: Ident list): R2nResult<'a> =
        // Add edges from stateIdx to each state in destStates
        let addDestEdges stateIdx destStates nfa =
            List.fold (fun machine state -> machine |> add (at stateIdx machine) (Some state)) nfa destStates

        match p, i, destinationStates with
        | Empty, n, _  -> {Machine = emptyNfa; NextIdent = n; Bypassable = false}
//        | Star Empty, n, _    // Can accept no input: gross way of writing Eps.
//        | Star Eps, n, _      // Effectively Eps.
        | Star (Eps' _), n, _ // Can occur in partially derived parsers. While these are epsilon edges, we can't remove them from a parser, because they contain (partial) parse trees
        | Eps, n, _           // We quietly elide Eps/Eps' from the resulting NFA.
        | Eps' _, n, _ -> {Machine = emptyNfa; NextIdent = n; Bypassable = true}
        | Char c, n, dest -> {Machine = {((emptyNfa |> add {Ident = n; Token = c} None) |> addDestEdges n dest) with
                                            Starts = [n]
                                            Finals = [n]}
                              NextIdent = n + 1
                              Bypassable = false}
        | Union (x, y), n, ds ->
            let right = r2n' y n ds
            let left = r2n' x right.NextIdent ds
            {Machine = {merge left.Machine right.Machine with
                            Starts = List.append right.Machine.Starts left.Machine.Starts
                            Finals = List.append right.Machine.Finals left.Machine.Finals}
             NextIdent = left.NextIdent
             Bypassable = right.Bypassable || left.Bypassable}
        | Cat (x, y), n, ds ->
            let second = r2n' y n ds
            let first = r2n' x second.NextIdent second.Machine.Starts
            {Machine = {merge first.Machine second.Machine with
                            Starts = first.Machine.Starts
                            Finals = second.Machine.Finals}
             NextIdent = first.NextIdent
             Bypassable = first.Bypassable && second.Bypassable}
        | Star x, n, ds ->
            let inner = r2n' x n ds
            let mutable newMachine = inner.Machine
            // We want to make ALL FINAL states in inner route to all start states in the result.
            for newEdge in (destStateIdxs 0 inner.Machine) do // Direct reference to final token!
                newMachine <- newMachine |> addDestEdges newEdge inner.Machine.Starts |> addDestEdges newEdge ds
            {Machine = {(newMachine |> addDestEdges n inner.Machine.Starts |> addDestEdges n ds) with
                         Starts = List.append inner.Machine.Starts ds
                         Finals = List.append inner.Machine.Finals ds}
             NextIdent = inner.NextIdent
             Bypassable = true}

    let r2n (r: Parser<'a>) : NFA<'a> =
        let terminatingState = {Ident = 0; Token = Unchecked.defaultof<'a>} // Direct reference to final token!
        let compactedParser = compact r // Remove junk like "Star Empty" or "Star Eps"
        let result = r2n' compactedParser 1 [terminatingState.Ident] // "1" because it's the successor to terminatingState.Ident
        // r2n' doesn't add the terminating state because an Empty NFA has no edges (therefore no states)
        let machineWithTerminator = result.Machine |> add terminatingState None
        machineWithTerminator

    let accept (ds: Ident seq): bool =
        ds |> Seq.exists (fun d -> d = 0) // Direct reference to final token!

    type Walk = {ReversePath: Ident list}
    // Yield a sequence of _paths_ through a NFA. The paths are REVERSED. This
    // lets users determine, in constant time, the last node reached. (For example,
    // checking acceptance of a word in an NFA representing a regular expression.)
    let bfsPath (startIdxs: Ident seq) (nfa: NFA<'a>) =
        let sortedByToken nfa idents =
            idents
            |> Seq.map (fun idx -> at idx nfa)
            |> Seq.sortBy (fun state -> state.Token)
            |> Seq.map (fun state -> state.Ident)
        let nextSteps (walk: Walk) (nfa: NFA<'a>) =
                edgesOf (List.head walk.ReversePath) nfa
                |> (sortedByToken nfa)
                |> Seq.map (fun next -> {ReversePath = next::walk.ReversePath})
        let initials = startIdxs
                       |> (sortedByToken nfa)
                       |> Seq.map (fun start -> {ReversePath = [start]})
        seq {
            // Locally mutable queue = blazingly fast.
            let queue = new System.Collections.Generic.List<Walk>();
            queue.AddRange(initials)

            while queue.Count > 0 do
                let currentPath = queue.[0]
                queue.RemoveAt(0)
                yield currentPath.ReversePath
                let nextPaths = nextSteps currentPath nfa
                queue.AddRange(nextPaths)
        }

    // Yield each node in nfa in breadth-first order.
    let bfsi (nfa: NFA<'a>) =
        let nextSteps (nodeIdx: Ident) (nfa: NFA<'a>) =
            edgesOf nodeIdx nfa
        seq {
                let queue = ref(Seq.singleton (start nfa).Ident)
                while not(Seq.isEmpty !queue) do
                    let currentIdx = Seq.head !queue
                    queue := Seq.skip 1 !queue
                    yield currentIdx
                    let nextEdges = nextSteps currentIdx nfa
                    queue := Seq.append !queue nextEdges
            }

    // generate generates every word in the language described by a parser p.
    // It does so in a fair manner, in that the union of two parsers a and b
    // will return words from a and b, in order.
    let generate (p: Parser<'a>): 'a list seq =
        let rec word path =
            match path with
            | 0::_ -> true
            | _::_
            | []    -> false
        let toToken nfa path =
            (match path with
            | 0::xs -> xs
            | _::_
            | [] -> []) // Shouldn't be able to generate this path...
            |> List.map (fun idx -> (at idx nfa).Token)
            |> List.rev
        let nfa = r2n p
        nfa.Starts
        |> (fun starts -> (bfsPath starts nfa))
        |> Seq.filter word
        |> Seq.map (toToken nfa)
        |> Seq.distinct

    // matches returns true if a parser matches the entire input.
    let rec matches p = function
        | []    -> nullable p
        | x::xs ->
            let deriv = (d x p)
            let c = compact deriv
            matches c xs

    // matchSeq returns true if a parser matches the entire input seq. Useful for matching Strings.
    let matchSeq p s =
        List.ofSeq s
        |> matches p

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

    // all is like Cat, but for n tokens, not just two.
    let rec all tokens =
        match tokens with
        | []    -> Empty
        | [x]   -> Char x
        | x::xs -> Cat (Char x, all xs)

    // rep returns a parser that accepts count repetitions of any word in
    // p's language.
    let rec rep count p =
        match count with
        | 0 -> Eps
        | 1 -> p
        | n  -> Cat (p, rep (n - 1) p)

    // atLeast returns a parser that accepts at least count number of repetitions
    // of any word in p's language.
    let atLeast count p =
        match count with
        | x when x < 0 -> invalidArg "count" "min >= 0"
        | _            -> Cat (rep count p, Star p)

    // reps returns a parser that accepts anywhere between min and max number
    // of repetitions of any word in p's language.
    let reps min max p =
        match min,max with
        | l,_ when l < 0 -> invalidArg "min" "min >= 0"
        | l,u when u < l -> invalidArg "max" "max >= min"
        | l,u            -> seq { l..u }
                            |> Seq.map (fun n -> rep n p)
                            |> Seq.reduce (fun acc each -> Union (acc, each))

    // atMost returns a parser that accepts any number (including zero) of repetitions
    // of any word in p's language, up to some maximum amount.
    let atMost count p =
        reps 0 count p

    // opt returns a parser that accepts a word in some parser's language, or nothing.
    // It's the ? operator in most regex languages.
    let opt p = atMost 1 p

    // ParsePosition tracks our progress in parsing: it stores the original parser,
    // the parsers we have derived thus far, and the parse trees of those parsers.
    type ParsePosition<'a when 'a: comparison> = {
        Parser: Parser<'a>
        CurrentParsers: Parser<'a> list
        Parses: 'a list seq}

    // gatherParses returns all parse trees for _completed_ parses, i.e.,
    // parsers that can process an empty input. It returns unique parses.
    let gatherParses parsers =
        parsers
        |> List.filter nullable // Only handle parsers with complete parses
        |> List.map parseNull   // Gather all parses
        |> List.map Set.toList  // Into one big list
        |> List.concat          // Glom them together
        |> List.toSeq           // And spit out a lazy list

    // startFind gives you an easy way to start matching over partial input.
    let startFind parser: ParsePosition<'a> =
        {Parser = parser
         CurrentParsers = []
         Parses = Seq.empty}

    // resumableFind lets you pause parsing while you wait for more input.
    let rec resumableFind (parsePosition: ParsePosition<'a>) input: ParsePosition<'a> =
        let baseParser, parsers, parses = parsePosition.Parser, parsePosition.CurrentParsers, parsePosition.Parses
        // Star parsers need to "munch maximally", matching against as much
        // input as they can. Thus we need to find such parsers. Any parser
        // that contains a Star may munch maximally.
        let rec star = function
            | Empty | Eps | Eps' _ | Char _ -> false
            | Star _ -> true
            | Union (a, b) -> star a || star b
            | Cat (a, b)   -> star a || star b
        let reject predicate l = List.filter (fun x -> not (predicate x)) l
        if (Seq.isEmpty input) then
            // If there's no more input, _do nothing_. Either the entire
            // input has been processed, or we have exhausted the input
            // temporarily. Return the current parse state.
            {Parser = baseParser
             CurrentParsers = parsers
             Parses = parses}
        else
            let x = Seq.head input
            let xs = Seq.skip 1 input
            let endingParsers, rest = parsers
                                        |> reject empty
                                        |> List.partition (fun p -> dP x p |> empty)
            let newParsers = (match (star baseParser, rest) with
                                | false, []
                                | false, _::_
                                | true,  [] -> baseParser::rest // Add a new parser for non-maximal munchers
                                | true,  _::_ -> rest)          // Otherwise, just keep the old one ticking along.
                                |> List.map (fun p -> dP x p)
                                |> List.map compact
                                |> reject empty
            let maximalParses = endingParsers
                                |> List.map compact
                                |> gatherParses
                                |> Seq.append parses    // And add the new parses to the pile.
                                                        // Don't dedupe here, because duplicate parses
                                                        // will start from a new point in the input!
            let newParses = rest
                            |> reject star
                            |> gatherParses
                            |> Seq.append maximalParses
            resumableFind {Parser = baseParser; CurrentParsers = newParsers; Parses = newParses} xs

    // finishFind finishes off a resumable find: now that you have no more
    // input, what are the final parse trees?
    // parses contains any complete parses found thus far; parses contains
    // any remaining complete parses.
    let finishFind (parsePosition: ParsePosition<'a>) =
        parsePosition.CurrentParsers
        |> gatherParses
        |> Seq.append parsePosition.Parses

    // find finds all matches in some list.
    // In essence, at each step, find
    // * adds the original parser to the list of running parsers,
    // * derives all currently running parsers with respect to the next element,
    // * culls Empty parsers,
    // * collects the parse trees of any parsers
    // * stores maximal-munches.
    let find p (s: 'a seq): 'a list seq =
        // TODO: but we want to record our position in the stream, which means we
        // must throw away the idea of walking over a naked list: we need to count characters
        // and such!
        resumableFind (startFind p) s
        |> finishFind

    let findMatches p s = find p (List.ofSeq s)