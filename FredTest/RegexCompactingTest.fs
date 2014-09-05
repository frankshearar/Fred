module Regex.Test.Compacting

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Compacting``() =
    [<Test>]
    member x.``Empty is compact``() =
        Assert.AreEqual(Empty, compact Empty)
    [<Test>]
    member x.``Eps is compact``() =
        Assert.AreEqual(Eps, compact (Eps))
    [<Test>]
    member x.``Eps [] is compact``() =
        Assert.AreEqual(Eps' (set [[]]), compact (Eps' (set [[]])))
    [<Test>]
    member x.``Char is compact``() =
        Assert.AreEqual(Char 'a', compact (Char 'a'))
    [<Test>]
    member x.``Empty parsers compact to Empty``() =
        Assert.AreEqual(Empty, compact (Union (Empty, Empty)))
        Assert.AreEqual(Empty, compact (Cat (Empty, Empty)))
    [<Test>]
    member x.``Star Empty = Eps``() =
        Assert.AreEqual(Eps, compact (Star Empty))
    [<Test>]
    member x.``prunes empty subparsers``() =
        Assert.AreEqual(Char 'a', compact (Union (Empty, Char 'a')))
        Assert.AreEqual(Char 'a', compact (Union (Char 'a', Empty)))
    [<Test>]
    member x.``compacts non-empty subparsers``() =
        Assert.AreEqual(Char 'a', compact (Union (Empty, Union (Union (Empty, Empty), Char 'a'))))
        Assert.AreEqual(Char 'a', compact (Union (Union (Union (Empty, Empty), Char 'a'), Empty)))
    [<Test>]
    member x.``Empty then something is empty``() =
        let empty: Parser<char> = Empty
        Assert.AreEqual(empty, compact (Cat (Empty, Char 'a')))
    [<Test>]
    member x.``Something then empty is empty``() =
        let empty: Parser<char> = Empty
        Assert.AreEqual(empty, compact (Cat (Char 'a', Empty)))
    [<Test>]
    member x.``removes nullable trailing subparsers``() =
        Assert.AreEqual(Char 'a', compact (Cat (Char 'a', Cat (Eps, Cat (Eps, Eps)))))
    [<Test>]
    member x.``does not remove nullable trailing subparsers with partial parse trees``() =
        let p = (Cat (Char 'a', Cat ((Eps' (set [[]])), Cat ((Eps' (set [[]])), (Eps' (set [[]]))))))
        Assert.AreEqual(p, compact p)
    [<Test>]
    member x.``Nothing then something is something``() =
        Assert.AreEqual(Char 'a', compact (Cat (Eps, Char 'a')))
    [<Test>]
    member x.``Nothing then something is something, and compact``() =
        Assert.AreEqual(Char 'a', compact (Cat (Eps, Cat (Eps, Char 'a'))))
    [<Test>]
    member x.``Something then nothing is something``() =
        Assert.AreEqual(Char 'a', compact (Cat (Char 'a', Eps)))
    [<Test>]
    member x.``Something then nothing is something, and compact``() =
        Assert.AreEqual(Char 'a', compact (Cat (Char 'a', Cat (Eps, Eps))))
    [<Test>]
    member x.``Nothing then nothing is nothing``() =
        Assert.AreEqual(Eps, compact (Cat (Eps, Eps)))
    [<Test>]
    member x.``Something in between nothing is something``() =
        Assert.AreEqual(Char 'a', compact (Cat (Eps, Cat (Char 'a', Eps))))
    [<Test>]
    member x.``compacts sequences of parsers``() =
        Assert.AreEqual(Cat (Char 'a', Char 'b'), compact (Cat (Cat (Eps, Char 'a'), Cat(Eps, Char 'b'))))
    [<Test>]
    member x.``of Star of Eps is Eps``() =
        Assert.AreEqual(Eps, compact (Star Eps))
    [<Test>]
    member x.``of a Union should return a compact parser``() =
        Assert.AreEqual(Eps, compact (Union (Eps, Star Empty)))
    [<Test>]
    member x.``of a Union of two identical parser should return that parser, compacted``() =
        Assert.AreEqual(Eps, compact (Union (Star Empty, Star Empty)))
    [<Test>]
    member x.``Eps then something nullable is that nullable subparser``() =
        Assert.AreEqual((Star (Char 'a')), (compact (Cat (Eps, (Star (Char 'a'))))))
    [<Test>]
    member x.``of a compacted parser is itself``() =
        let compactedParserIsCompact (p: Parser<int>) =
            let cp = compact p
            cp = compact cp
        Check.QuickThrowOnFailure compactedParserIsCompact
    [<Test>]
    member x.``does not alter the accepted language``() =
        let takeSome seq =
            seq
            |> Seq.truncate 1000
            |> List.ofSeq
        let langUnaltered (p: Parser<int>) =
            takeSome (generate p) = takeSome (generate (compact p))
        Check.QuickThrowOnFailure langUnaltered