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
    member x.``Empty``() =
        Assert.False(matches Empty [])
    [<Test>]
    member x.``Eps matches empty input``() =
        Assert.That(matches Eps [])
    [<Test>]
    member x.``Eps' [] matches empty input``() =
        Assert.That(matches (Eps' (set [[]])) [])
    [<Test>]
    member x.``Eps' [] doesn't match non-empty input``() =
        Assert.False(matches (Eps' (set [[]])) ['a'])
    [<Test>]
    member x.``Char matches own token``() =
        Assert.That(matches (Char 'a') ['a'])
        Assert.False(matches (Char 'a') ['b'])
    [<Test>]
    member x.``Char doesn't match left over input``() =
        Assert.False(matches (Char 'a') ['a'; 'b'])
    [<Test>]
    member x.``Union matches with either subparser``() =
        let union = Union (Char 'a', Char 'b')
        Assert.That(matches union ['a'])
        Assert.That(matches union ['b'])
        Assert.False(matches union ['c'])
    [<Test>]
    member x.``Union doesn't match left over input``() =
        let union = Union (Char 'a', Char 'b')
        Assert.False(matches union ['c'])
    [<Test>]
    member x.``Cat matches with both parsers``() =
        let cat = Cat (Char 'a', Char 'b')
        Assert.That(matches cat ['a'; 'b'])
    [<Test>]
    member x.``Cat doesn't match short input``() =
        let cat = Cat (Char 'a', Char 'b')
        Assert.False(matches cat ['a'])
    [<Test>]
    member x.``Cat doesn't match unexpected input``() =
        let cat = Cat (Char 'a', Char 'b')
        Assert.False(matches cat ['b'; 'b'])
        Assert.False(matches cat ['a'; 'a'])
    [<Test>]
    member x.``Cat doesn't match left over input``() =
        let cat = Cat (Char 'a', Char 'b')
        Assert.False(matches cat ['a'; 'b'; 'c'])
    [<Test>]
    member x.``Star matches empty input``() =
        Assert.That(matches (Star (Char 'a')) [])
    [<Test>]
    member x.``Star matches repeated elements``() =
        let star = Star (Char 'a')
        Assert.That(matches star ['a'])
        Assert.That(matches star ['a'; 'a'])
        Assert.That(matches star ['a'; 'a'; 'a'])
    [<Test>]
    member x.``Star doesn't match unexpected input``() =
        let star = Star (Char 'a')
        Assert.False(matches star ['b'])

[<TestFixture>]
type ``Testing any``() =
    [<Test>]
    member x.``should accept any of the listed tokens``() =
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
    member x.``with no tokens means Empty``() =
        Assert.AreEqual(Empty, any [])
    [<Test>]
    member x.``with single token means Char``() =
        let oneMeansChar (x: char) = // We need a concrete type because Geneflect doesn't handle IComparable
            match any [x] with
            | Char y -> x = y
            | _      -> false
        Check.QuickThrowOnFailure oneMeansChar

[<TestFixture>]
type ``Testing all``() =
    [<Test>]
    member x.``with no tokens means Empty``() =
        Assert.AreEqual(Empty, all [])
    [<Test>]
    member x.``with one token means Char``() =
        let oneMeansChar (x: char) = // We need a concrete type because Geneflect doesn't handle IComparable
            match all [x] with
            | Char y -> x = y
            | _      -> false
        Check.QuickThrowOnFailure oneMeansChar
    [<Test>]
    member x.``with multiple tokens means Cat``() =
        let manyMeansCat (xs: char list) =
            not (List.isEmpty xs) ==>
            matches (all xs) xs
        Check.QuickThrowOnFailure manyMeansCat

[<TestFixture>]
type ``Test find``() =
    [<Test>]
    member x.``with no input returns no results, regardless of parser``() =
        let neverAnyResultsForNoInput (p: Parser<char>) =
            let matches = find p [] |> List.ofSeq
            listEqual [] matches
        Check.QuickThrowOnFailure neverAnyResultsForNoInput
    [<Test>]
    member x.``with input, never returns an empty match``() =
        let neverReturnAnEmpty (p: Parser<char>) (input: char list) =
            not (List.isEmpty input) ==>
            let matches = find p input |> List.ofSeq
            None = List.tryFind List.isEmpty matches
        Check.QuickThrowOnFailure neverReturnAnEmpty
    [<Test>]
    member x.``can find a single match``() =
        let hit = ['a';'b';'c']
        let matches = find (all hit) ['a'..'z'] |> List.ofSeq
        listEqual [hit] matches
    [<Test>]
    member x.``can return multiple matches``() =
        let hit = ['a';'b';'c']
        let matches = find (all hit) (['a'..'z']@['a'..'z']) |> List.ofSeq
        listEqual [hit;hit] matches
    [<Test>]
    member x.``can find single-char matches at the start of input``() =
        let matches = find (Char 'a') ("abababababbaabbbabbababa" |> List.ofSeq) |> List.ofSeq
        listEqual ['a'] (List.head matches)
        Assert.AreEqual(11, List.length matches)
    [<Test>]
    member x.``can find overlapping matches``() =
        let hit = ['a'; 'b'; 'a']
        let matches = find (all hit) ("abababababbaabbbabbababa" |> List.ofSeq) |> List.ofSeq
        listEqual hit (List.head matches)
        Assert.AreEqual(6, List.length matches)