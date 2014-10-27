module Regex.Test.ParseTrees

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Building parse trees``() =
    [<Test>]
    member x.``from Eps should yield sole parse tree``() =
        let justOneParse (t: char list) =
            set [t] = (parseNull (Eps' (set [t])))
        Check.QuickThrowOnFailure justOneParse
    [<Test>]
    member x.``from Empty should yield no parses``() =
        Assert.AreEqual(Set.empty, (parseNull Empty))
    [<Test>]
    member x.``from Char should yield no parses``() =
        let noParses (t: char) =
            Set.isEmpty (parseNull (Char t))
        Check.QuickThrowOnFailure noParses
    [<Test>]
    member x.``from Union should yield subparsers' parse trees``() =
        let setUnion (a: Parser<int>) b =
            (parseNull (Union (a, b))) = Set.union (parseNull a) (parseNull b)
        Check.QuickThrowOnFailure setUnion
    [<Test>]
    member x.``from Cat should yield parse trees assembled from subparsers' parse trees``() =
        let parsesIncludesFirstParsesThenEps (p1: Parser<int>) p2 =
            let firstParses = parseNull p1
            let secondParses = parseNull p2
            let catParses = parseNull (Cat (p1, p2)) |> List.ofSeq
            seq {for first in firstParses do
                     for second in secondParses do
                         yield List.append first second}
            |> Seq.map (fun parse -> List.exists (fun each -> each = parse) catParses)
            |> Seq.fold (&&) true
        Check.QuickThrowOnFailure parsesIncludesFirstParsesThenEps
    [<Test>]
    member x.``from Cat should yield concatenated parses even if second parser has none``() =
        Assert.AreEqual(set [['a']], (parseNull (Cat (Eps' (set [['a']]),Star (Char 'a')))))
    [<Test>]
    member x.``Not is self-negating``() =
        let notIsOwnInverse (p: Parser<char>) =
            parseNull p = parseNull (Not (Not p))
        Check.QuickThrowOnFailure notIsOwnInverse