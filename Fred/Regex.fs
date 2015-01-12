﻿namespace Fred

// TODO:
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

// A Length Ordered List wraps a List<'a> such that a shorter list is less
// than a longer list, and lists of equal length are ordered by the compare
// of their heads.
module LOL =
    // Given two Length Ordered Lists a and b, a < b if a is shorter than b.
    [<CustomComparison>]
    [<CustomEquality>]
    type LOL<'a> when 'a: comparison =
        | LOL of 'a list
        interface System.IComparable with
            member x.CompareTo(obj: System.Object) =
                // At this point we can't use the public unwrap!
                // Using a private member looks even worse than this.
                let unwrap lol = match lol with | LOL xs' -> xs'
                let rec comp xs ys =
                    // By construction, equal-length lists.
                    match xs,ys with
                    | [], []      -> 0
                    | [],y::yt    -> -1
                    | x::xt,[]    -> 1
                    | x::xt,y::yt -> let comparedTails = comp xt yt
                                     if comparedTails = 0 then compare x y else comparedTails
                match obj with
                | :? LOL<'a> as ys ->
                    comp (unwrap x) (unwrap ys)
                | _ -> -1
        override x.Equals(obj: System.Object) =
            (x :> System.IComparable).CompareTo(obj) = 0
        override x.GetHashCode() =
            match x with | LOL xs -> xs.GetHashCode()

    let append x y =
        match x, y with
        | LOL xs, LOL ys -> LOL (List.append xs ys)

    let over xs = LOL xs

    let unwrap = function
        | LOL x -> x

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

    let (|LT|EQ|GT|) (a, b) =
        match compare a b with
        | 0 -> EQ
        | n when n < 0 -> LT
        | _ -> GT

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

    // Given two sequences and a function, xprod returns the result of applying
    // that function to every pair of values in the sequences.
    // It returns values in the order defined by the elements' CompareTo.
    let rec xprod f xs ys =
        match Seq.isEmpty xs, Seq.isEmpty ys with
        | true,  true
        | false, true
        | true,  false -> Seq.empty
        | false, false ->
            // Pulling out the tails in let bindings looks neater,
            // but since we use xprod A LOT, let's defer the creation
            // of those tails right until just before we need them.
            let x = Seq.head xs
            let y = Seq.head ys
            seq {
                 yield f x y
                 yield! xprod f (Seq.singleton x) (Seq.skip 1 ys) |/ (xprod f (Seq.skip 1 xs) ys)
                }

    // closure calculates the Kleene closure x* of a function, a least element and
    // a set of strings: closure is the fixed point of k = z | f(k).
    let rec closure f z xs =
        if Seq.isEmpty xs then
            Seq.singleton z
        else
            let x = Seq.head xs
            if x = z then
                closure f z (Seq.skip 1 xs)
            else
                // Using seq {} here simulates the laziness of Haskell's :: operator.
                seq {
                     yield z;
                     yield! xprod f xs (closure f z xs)
                    }

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
                         match (Seq.isEmpty prefix, Seq.isEmpty suffix) with
                         | true, true  -> Set.empty
                         | false, true -> prefix
                         | true, false -> suffix
                         | false, false -> xprod List.append prefix suffix
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

    // generate generates every word in the language described by a parser p.
    // It does so in a fair manner, in that the union of two parsers a and b
    // will return words from a and b, in order.
    let generate p =
        let rec gen = function
        | Empty       -> Seq.empty
        | Eps
        | Eps' _      -> seq { yield LOL.over [] } // generate doesn't use the parse trees.
        | Char c      -> seq { yield LOL.over [c] }
        | Union (a,b) -> (gen a) |/ (gen b)
        | Cat (a,b)   -> xprod LOL.append (gen a) (gen b)
        | Star a      -> closure LOL.append (LOL.over []) (gen a)
        gen p |> Seq.map LOL.unwrap

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
                                                 | Empty | Eps | Eps' _ | Char _ -> parent // Effectively a no-op
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
        | l,u when l < 0 -> invalidArg "min" "min >= 0"
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
                                | true,  p::ps -> rest)         // Otherwise, just keep the old one ticking along.
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