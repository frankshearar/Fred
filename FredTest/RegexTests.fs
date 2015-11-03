module Regex.Test.Matching

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Matching``() =
    [<Test>]
    member __.``Empty``() =
        Assert.False(matches Empty [])
    [<Test>]
    member __.``Eps matches empty input``() =
        Assert.That(matches Eps [])
    [<Test>]
    member __.``Eps' [] matches empty input``() =
        Assert.That(matches (Eps' (set [[]])) [])
    [<Test>]
    member __.``Eps' [] doesn't match non-empty input``() =
        Assert.False(matches (Eps' (set [[]])) ['a'])
    [<Test>]
    member __.``Char matches own token``() =
        Assert.That(matches (Char 'a') ['a'])
        Assert.False(matches (Char 'a') ['b'])
    [<Test>]
    member __.``Char doesn't match left over input``() =
        Assert.False(matches (Char 'a') ['a'; 'b'])
    [<Test>]
    member __.``Union matches with either subparser``() =
        let union = Union (Char 'a', Char 'b')
        Assert.That(matches union ['a'])
        Assert.That(matches union ['b'])
        Assert.False(matches union ['c'])
    [<Test>]
    member __.``Union doesn't match left over input``() =
        let union = Union (Char 'a', Char 'b')
        Assert.False(matches union ['c'])
    [<Test>]
    member __.``Cat matches with both parsers``() =
        let cat = Cat (Char 'a', Char 'b')
        Assert.That(matches cat ['a'; 'b'])
    [<Test>]
    member __.``Cat doesn't match short input``() =
        let cat = Cat (Char 'a', Char 'b')
        Assert.False(matches cat ['a'])
    [<Test>]
    member __.``Cat doesn't match unexpected input``() =
        let cat = Cat (Char 'a', Char 'b')
        Assert.False(matches cat ['b'; 'b'])
        Assert.False(matches cat ['a'; 'a'])
    [<Test>]
    member __.``Cat doesn't match left over input``() =
        let cat = Cat (Char 'a', Char 'b')
        Assert.False(matches cat ['a'; 'b'; 'c'])
    [<Test>]
    member __.``Star matches empty input``() =
        Assert.That(matches (Star (Char 'a')) [])
    [<Test>]
    member __.``Star matches repeated elements``() =
        let star = Star (Char 'a')
        Assert.That(matches star ['a'])
        Assert.That(matches star ['a'; 'a'])
        Assert.That(matches star ['a'; 'a'; 'a'])
    [<Test>]
    member __.``Star doesn't match unexpected input``() =
        let star = Star (Char 'a')
        Assert.False(matches star ['b'])
    [<Test>]
    member __.``matchSeq matches against seqs``() =
        let star = Star (Char 'a')
        Assert.That(matchSeq star (Seq.singleton 'a'))
        Assert.That(matchSeq star (Seq.init 2 (fun _ -> 'a')))
        Assert.That(matchSeq star (Seq.init 3 (fun _ -> 'a')))

[<TestFixture>]
type ``Testing any``() =
    [<Test>]
    member __.``should accept any of the listed tokens``() =
        let nonEmptyAcceptsAnyAlternative (tokens: char list) = // We need a concrete type because Geneflect doesn't handle IComparable
            not (List.isEmpty tokens) ==>
            let p = any tokens
            tokens
            |> List.map (fun input -> d input p)             // The derivative wrt to any of the tokens will be some chain of Unions.
                                                             // All but one leaf parser will be Empty,
            |> List.map compact                              // so compact will turn that tree into an Eps...
            |> List.map (fun derivative -> derivative = Eps) // ...  but is that what happened?
            |> List.fold (&&) true
        Check.QuickThrowOnFailure nonEmptyAcceptsAnyAlternative
    [<Test>]
    member __.``with no tokens means Empty``() =
        parserEqual Empty (any [])
    [<Test>]
    member __.``with single token means Char``() =
        let oneMeansChar (x: char) = // We need a concrete type because Geneflect doesn't handle IComparable
            match any [x] with
            | Char y -> x = y
            | _      -> false
        Check.QuickThrowOnFailure oneMeansChar

[<TestFixture>]
type ``Testing all``() =
    [<Test>]
    member __.``with no tokens means Empty``() =
        parserEqual Empty (all [])
    [<Test>]
    member __.``with one token means Char``() =
        let oneMeansChar (x: char) = // We need a concrete type because Geneflect doesn't handle IComparable
            match all [x] with
            | Char y -> x = y
            | _      -> false
        Check.QuickThrowOnFailure oneMeansChar
    [<Test>]
    member __.``with multiple tokens means Cat``() =
        let manyMeansCat (xs: char list) =
            not (List.isEmpty xs) ==>
            matches (all xs) xs
        Check.QuickThrowOnFailure manyMeansCat

[<TestFixture>]
type ``rep parser``() =
    let threeAs = rep 3 (Char 'a')
    [<Test>]
    member __.``matches 0 repetitions of a parser through Eps``() =
        parserEqual Eps (rep 0 (Char 'a'))
    [<Test>]
    member __.``matches 1 repetition of a parser through identity``() =
        parserEqual (Char 'a') (rep 1 (Char 'a'))
    [<Test>]
    member __.``matches N repetitions of a parser``() =
        Assert.That(matches threeAs (List.replicate 3 'a'))
    [<Test>]
    member __.``doesn't match N+M, M > 0, repetitions of a parser``() =
        Assert.False(matches threeAs (List.replicate 2 'a'))
    [<Test>]
    member __.``doesn't match N+M, 0 < M < N, repetitions of a parser``() =
        Assert.False(matches threeAs (List.replicate 4 'a'))

[<TestFixture>]
type ``atLeast parser``() =
    let atLeastThreeAs = atLeast 3 (Char 'a')
    [<Test>]
    member __.``matches N repetitions of a parser``() =
        Assert.That(matches atLeastThreeAs (List.replicate 3 'a'))
    [<Test>]
    member __.``matches N+M, M > 0, repetitions of a parser``() =
        Assert.That(matches atLeastThreeAs (List.replicate 6 'a'))
    [<Test>]
    member __.``doesn't match N-M, 0 < M < N, repetitions of a parser``() =
        Assert.False(matches atLeastThreeAs (List.replicate 2 'a'))
    [<Test>]
    member __.``requires a non-negative count``() =
        Assert.Throws<System.ArgumentException>(fun () -> atLeast -1 (Char 'a') |> ignore) |> ignore

[<TestFixture>]
type ``atMost parser``() =
    let atMostThreeAs = atMost 3 (Char 'a')
    [<Test>]
    member __.``matches 0 repetitions of a parser``() =
        Assert.That(matches atMostThreeAs [])
    [<Test>]
    member __.``matches N-M, 0 < M < N, repetitions of a parser``() =
        Assert.That(matches atMostThreeAs (List.replicate 1 'a'))
        Assert.That(matches atMostThreeAs (List.replicate 2 'a'))
    [<Test>]
    member __.``matches N repetitions of a parser``() =
        Assert.That(matches atMostThreeAs (List.replicate 3 'a'))
    [<Test>]
    member __.``does not match N+M, M > 0, repetitions of a parser``() =
        Assert.False(matches atMostThreeAs (List.replicate 4 'a'))

[<TestFixture>]
type ``alpha parser``() =
    let alphabet = seq {
                        yield! seq {'a' .. 'z'}
                        yield! seq {'A' .. 'Z'}
                        }
    [<Test>]
    member __.``accepts all English alphabet characters``() =
        Assert.That(matches (Star alpha) (alphabet |> List.ofSeq))
    [<Test>]
    member __.``accepts no other input``() =
        // Hardly an exhaustive list, but generating chars and filtering out the unacceptable
        // chars is pretty expensive.
        Assert.False(matches alpha ['0'])
        Assert.False(matches alpha ['|'])
        Assert.False(matches alpha ['é'])
        Assert.False(matches alpha [' '])

[<TestFixture>]
type ``alphanum parser``() =
    let alphabetOrDigit = seq {
                        yield! seq {'a' .. 'z'}
                        yield! seq {'A' .. 'Z'}
                        yield! seq {'0' .. '9'}
                        }
    [<Test>]
    member __.``accepts all English alphabet characters, and digits``() =
        Assert.That(matches (Star alphanum) (alphabetOrDigit |> List.ofSeq))
    [<Test>]
    member __.``accepts no other input``() =
        // Hardly an exhaustive list, but generating chars and filtering out the unacceptable
        // chars is pretty expensive.
        Assert.False(matches alpha ['|'])
        Assert.False(matches alpha ['é'])
        Assert.False(matches alpha [' '])

[<TestFixture>]
type ``opt parser``() =
    [<Test>]
    member __.``accepts no input``() =
        Assert.True(matches (opt alpha) [])
    [<Test>]
    member __.``accepts a word from the parser's language``() =
        Assert.True(matches (opt alpha) ['a'])

[<TestFixture>]
type ``num parser``() =
    let digits = seq {'0' .. '9'}
    [<Test>]
    member __.``accepts all digit characters``() =
        Assert.That(matches (Star num) (digits |> List.ofSeq))
    [<Test>]
    member __.``accepts no other input``() =
        // Hardly an exhaustive list, but generating chars and filtering out the unacceptable
        // chars is pretty expensive.
        Assert.False(matches num ['a'])
        Assert.False(matches num ['Z'])
        Assert.False(matches num ['|'])
        Assert.False(matches num ['é'])
        Assert.False(matches num [' '])

[<TestFixture>]
type ``find function``() =
    [<Test>]
    member __.``with no input returns no results, regardless of parser``() =
        let neverAnyResultsForNoInput (p: Parser<char>) =
            let matches = find p [] |> List.ofSeq
            listEqual [] matches
        Check.QuickThrowOnFailure neverAnyResultsForNoInput
    [<Test>]
    member __.``with input, never returns an empty match``() =
        let neverReturnAnEmpty (p: Parser<char>) (input: char list) =
            not (List.isEmpty input) ==>
            let matches = find p input |> List.ofSeq
            None = List.tryFind List.isEmpty matches
        Check.QuickThrowOnFailure neverReturnAnEmpty
    [<Test>]
    member __.``can match entire input``() =
        let hit = ['a';'b';'c']
        let matches = find (all hit) hit |> List.ofSeq
        listEqual [hit] matches
    [<Test>]
    member __.``can find a single match``() =
        let hit = ['a';'b';'c']
        let matches = find (all hit) ['a'..'z'] |> List.ofSeq
        listEqual [hit] matches
    [<Test>]
    member __.``can return multiple matches``() =
        let hit = ['a';'b';'c']
        let matches = find (all hit) (['a'..'z']@['a'..'z']) |> List.ofSeq
        listEqual [hit;hit] matches
    [<Test>]
    member __.``can find single-char matches at the start of input``() =
        let matches = find (Char 'a') ("abababababbaabbbabbababa" |> List.ofSeq) |> List.ofSeq
        listEqual ['a'] (List.head matches)
        Assert.AreEqual(11, List.length matches)
    [<Test>]
    member __.``can find overlapping matches``() =
        let hit = ['a'; 'b'; 'a']
        let matches = find (all hit) ("abababababbaabbbabbababa" |> List.ofSeq) |> List.ofSeq
        listEqual hit (List.head matches)
        Assert.AreEqual(6, List.length matches)
    [<Test>]
    member __.``with Char can find matches``() =
        listEqual [['a']] ((find (Char 'a') ['a']) |> List.ofSeq)
        listEqual [['a']] ((find (Char 'a') ['b';'a']) |> List.ofSeq)
        listEqual [['a']] ((find (Char 'a') ['b';'a';'b']) |> List.ofSeq)
    [<Test>]
    member __.``with Star can find greedy/maximal matches``() =
        listEqual [['a';'a']] ((find (Star (Char 'a')) ['a';'a';'b']) |> List.ofSeq)
    [<Test>]
    member __.``with Star can find trailing matches``() =
        listEqual [['a';'a']] ((find (Star (Char 'a')) ['b';'a';'a']) |> List.ofSeq)
    [<Test>]
    member __.``Star doesn't match unexpected input``() =
        let star = Star (Char 'a')
        let matches = (find star ['b']) |> List.ofSeq
        listEqual [] matches

[<TestFixture>]
type ``findMatches function``() =
    [<Test>]
    member __.``matches seqs``() =
        seqEqual (Seq.singleton ['a']) ((findMatches (Char 'a') ['a']))

[<TestFixture>]
type ``Resumable find``() =
    [<Test>]
    member __.``can pause parsing``() =
        let position = resumableFind (startFind (all ['a';'b'])) "ab"
        let partialParses =
            finishFind position
            |> List.ofSeq
        listEqual [['a';'b']] partialParses
    [<Test>]
    member __.``can resume parsing``() =
        let pos1 = resumableFind (startFind (all ['a';'b'])) "ab"
        let pos2 = resumableFind pos1 "ab"
        let completedParses =
            finishFind pos2
            |> List.ofSeq
        listEqual [['a';'b'];['a';'b']]  completedParses