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
        parserEqual Empty (compact Empty)
    [<Test>]
    member x.``Eps is compact``() =
        parserEqual Eps (compact (Eps))
    [<Test>]
    member x.``Eps [] is compact``() =
        parserEqual (Eps' (set [[]])) (compact (Eps' (set [[]])))
    [<Test>]
    member x.``Char is compact``() =
        parserEqual (Char 'a') (compact (Char 'a'))
    [<Test>]
    member x.``Empty parsers compact to Empty``() =
        parserEqual (Empty) (compact (Union (Empty, Empty)))
        parserEqual (Empty) (compact (Cat (Empty, Empty)))
    [<Test>]
    member x.``Star Empty = Eps``() =
        parserEqual (Eps) (compact (Star Empty))
    [<Test>]
    member x.``prunes empty subparsers``() =
        parserEqual (Char 'a') (compact (Union (Empty, Char 'a')))
        parserEqual (Char 'a') (compact (Union (Char 'a', Empty)))
    [<Test>]
    member x.``compacts non-empty subparsers``() =
        parserEqual (Char 'a') (compact (Union (Empty, Union (Union (Empty, Empty), Char 'a'))))
        parserEqual (Char 'a') (compact (Union (Union (Union (Empty, Empty), Char 'a'), Empty)))
    [<Test>]
    member x.``Empty then something is empty``() =
        parserEqual Empty (compact (Cat (Empty, Char 'a')))
    [<Test>]
    member x.``Something then empty is empty``() =
        parserEqual Empty (compact (Cat (Char 'a', Empty)))
    [<Test>]
    member x.``removes nullable trailing subparsers``() =
        parserEqual (Char 'a') (compact (Cat (Char 'a', Cat (Eps, Cat (Eps, Eps)))))
    [<Test>]
    member x.``does not remove nullable trailing subparsers with partial parse trees``() =
        let p = (Cat (Char 'a', Cat ((Eps' (set [[]])), Cat ((Eps' (set [[]])), (Eps' (set [[]]))))))
        parserEqual p (compact p)
    [<Test>]
    member x.``Nothing then something is something``() =
        parserEqual (Char 'a') (compact (Cat (Eps, Char 'a')))
    [<Test>]
    member x.``Nothing then something is something, and compact``() =
        parserEqual (Char 'a') (compact (Cat (Eps, Cat (Eps, Char 'a'))))
    [<Test>]
    member x.``Something then nothing is something``() =
        parserEqual (Char 'a') (compact (Cat (Char 'a', Eps)))
    [<Test>]
    member x.``Something then nothing is something, and compact``() =
        parserEqual (Char 'a') (compact (Cat (Char 'a', Cat (Eps, Eps))))
    [<Test>]
    member x.``Nothing then nothing is nothing``() =
        parserEqual Eps (compact (Cat (Eps, Eps)))
    [<Test>]
    member x.``Something in between nothing is something``() =
        parserEqual (Char 'a') (compact (Cat (Eps, Cat (Char 'a', Eps))))
    [<Test>]
    member x.``compacts sequences of parsers``() =
        parserEqual (Cat (Char 'a', Char 'b')) (compact (Cat (Cat (Eps, Char 'a'), Cat(Eps, Char 'b'))))
    [<Test>]
    member x.``of Star of Eps is Eps``() =
        parserEqual Eps (compact (Star Eps))
    [<Test>]
    member x.``of a Union should return a compact parser``() =
        parserEqual Eps (compact (Union (Eps, Star Empty)))
    [<Test>]
    member x.``of a Union of two identical parser should return that parser, compacted``() =
        parserEqual Eps (compact (Union (Star Empty, Star Empty)))
    [<Test>]
    member x.``Eps then something nullable is that nullable subparser``() =
        parserEqual (Star (Char 'a')) ((compact (Cat (Eps, (Star (Char 'a'))))))
    [<Test>]
    member x.``of a compacted parser is itself``() =
        let compactedParserIsCompact (p: Parser<int>) =
            let cp = compact p
            cp = compact cp
        Check.QuickThrowOnFailure compactedParserIsCompact
    [<Test>]
    member x.``does not alter the accepted language``() =
        let langUnaltered (p: Parser<int>) =
            // This is a very small limit, because generating words from
            // Star parsers is something like exponential: on my laptop,
            // generating the 15th word of a* takes ~32 seconds! Checking
            // only the first 5 words isn't super great, but the test
            // should then reliably finish inside a minute.
            let limit = 5
            let expected = p |> generate |> Seq.truncate limit
            let actual = p |> compact |> generate |> Seq.truncate limit
            seqEqual expected actual
        Check.QuickThrowOnFailure langUnaltered