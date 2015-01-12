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
        setEqual Set.empty (parseNull Empty)
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
        let a = Eps' (set [['a']; ['b']])
        let b = Eps' (set [['1']; ['2']])
        let parses = parseNull (Cat (a, b))
        let expected = xprod List.append (parseNull a) (parseNull b) |> Set.ofSeq
        setEqual expected parses
    [<Test>]
    member x.``from Cat should yield concatenated parses even if second parser has none``() =
        setEqual (set [['a']]) (parseNull (Cat (Eps' (set [['a']]),Star (Char 'a'))))
