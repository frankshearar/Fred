module RegexTests

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
    [<Test>]
    member x.``Parsing empty string with an Eps gives no matches``() =
        Assert.AreEqual([], (findMatches Eps ""))
    [<Test>]
    member x.``Parsing empty string with an Eps [] gives no matches``() =
        Assert.AreEqual([], (findMatches (Eps' (set [[]])) ""))
    [<Test>]
    member x.``Parsing empty string with any parser gives no matches``() =
        Assert.AreEqual([], (findMatches (Char 'a') ""))
    [<Test>]
    member x.``Char parser can find matches``() =
        listEqual ["a"] (findMatches (Char 'a') "a")
        listEqual ["a"] (findMatches (Char 'a') "ba")
        listEqual ["a"] (findMatches (Char 'a') "bab")
    [<Test>]
    member x.``findMatches``() =
        listEqual [] (findMatches (Char 'a') "")
        listEqual ["a"] (findMatches (Char 'a') "a")
        listEqual ["a"; "a"; "a"; "a"] (findMatches (Char 'a') "aabbbbaa")
        listEqual ["ab"] (findMatches (Cat (Char 'a', Char 'b')) "ab")
        listEqual ["ab"] (findMatches (Cat (Char 'a', Char 'b')) "aabbbbaa")
        listEqual ["ab"; "ab"; "ab"] (findMatches (Cat (Char 'a', Char 'b')) "aabbbbaabbab")
        listEqual ["a"; "a"; "b"; "a"; "a"; "a"] (findMatches (Union (Char 'a', Char 'b')) "aabaaa")
        listEqual ["aa"; "aaa"] (findMatches (Star (Char 'a')) "aabaaa") // aka "Star is greedy"
    [<Test>]
    member x.``findSubMatch``() =
        let equal2 (a: obj * obj) b =
            match a,b with
            | (a1, a2), (b1, b2) ->
                listEqual a1 b1
                listEqual a2 b2
        equal2 ([],[])    (findSubMatch Empty (List.ofSeq ""))
        equal2 ([],['a']) (findSubMatch Empty (List.ofSeq "a"))
        equal2 ([],[])    (findSubMatch Eps (List.ofSeq ""))
        equal2 ([],['a']) (findSubMatch Eps (List.ofSeq "a"))
        equal2 ([],[])    (findSubMatch (Eps' (set [[]])) (List.ofSeq ""))
        equal2 ([],['a']) (findSubMatch (Eps' (set [[]])) (List.ofSeq "a"))
        equal2 ([], ['a'; 'a']) (findSubMatch (Cat (Char 'a', Char 'b')) (List.ofSeq "aa"))
    [<Test>]
    member x.``of a submatch loses no input``() =
        let noInputLost (p: Parser<int>) inputList =
            let matchedPrefix, remainder = findSubMatch p inputList
            (List.append matchedPrefix remainder) = inputList
        Check.QuickThrowOnFailure noInputLost

[<TestFixture>]
type ``Testing anyToken``() =
    [<Test>]
    member x.``should accept any of the listed tokens``() =
        let nonEmptyAcceptsAnyAlternative (tokens: char list) = // We need a concrete type because Geneflect doesn't handle IComparable
            not (List.isEmpty tokens) ==>
            let p = anyToken tokens
            tokens
            |> List.map (fun input -> d input p)             // The derivative wrt to any of the tokens will be some chain of Unions.
                                                             // All but one leaf parser will be Empty,
            |> List.map compact                              // so compact will turn that tree into an Eps...
            |> List.map (fun derivative -> derivative = Eps) // ...  but is that what happened?
            |> List.fold (&&) true
        Check.QuickThrowOnFailure nonEmptyAcceptsAnyAlternative
    [<Test>]
    member x.``with no tokens means Empty``() =
        Assert.AreEqual(Empty, anyToken [])
    [<Test>]
    member x.``with single token means Char``() =
        let oneMeansChar (x: char) = // We need a concrete type because Geneflect doesn't handle IComparable
            match anyToken [x] with
            | Char y -> x = y
            | _      -> false
        Check.QuickThrowOnFailure oneMeansChar

[<TestFixture>]
type ``Testing allTokens``() =
    [<Test>]
    member x.``with no tokens means Empty``() =
        Assert.AreEqual(Empty, allTokens [])
    [<Test>]
    member x.``with one token means Char``() =
        let oneMeansChar (x: char) = // We need a concrete type because Geneflect doesn't handle IComparable
            match allTokens [x] with
            | Char y -> x = y
            | _      -> false
        Check.QuickThrowOnFailure oneMeansChar
    [<Test>]
    member x.``with multiple tokens means Cat``() =
        let manyMeansCat (xs: char list) =
            not (List.isEmpty xs) ==>
            matches (allTokens xs) xs
        Check.QuickThrowOnFailure manyMeansCat

