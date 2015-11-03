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
    member __.``Empty is compact``() =
        parserEqual Empty (compact Empty)
    [<Test>]
    member __.``Eps is compact``() =
        parserEqual Eps (compact (Eps))
    [<Test>]
    member __.``Eps [] is compact``() =
        parserEqual (Eps' (set [[]])) (compact (Eps' (set [[]])))
    [<Test>]
    member __.``Char is compact``() =
        parserEqual (Char 'a') (compact (Char 'a'))
    [<Test>]
    member __.``Empty parsers compact to Empty``() =
        parserEqual (Empty) (compact (Union (Empty, Empty)))
        parserEqual (Empty) (compact (Cat (Empty, Empty)))
    [<Test>]
    member __.``Star Empty = Eps``() =
        parserEqual (Eps) (compact (Star Empty))
    [<Test>]
    member __.``prunes empty subparsers``() =
        parserEqual (Char 'a') (compact (Union (Empty, Char 'a')))
        parserEqual (Char 'a') (compact (Union (Char 'a', Empty)))
    [<Test>]
    member __.``compacts non-empty subparsers``() =
        parserEqual (Char 'a') (compact (Union (Empty, Union (Union (Empty, Empty), Char 'a'))))
        parserEqual (Char 'a') (compact (Union (Union (Union (Empty, Empty), Char 'a'), Empty)))
    [<Test>]
    member __.``Empty then something is empty``() =
        parserEqual Empty (compact (Cat (Empty, Char 'a')))
    [<Test>]
    member __.``Something then empty is empty``() =
        parserEqual Empty (compact (Cat (Char 'a', Empty)))
    [<Test>]
    member __.``removes nullable trailing subparsers``() =
        parserEqual (Char 'a') (compact (Cat (Char 'a', Cat (Eps, Cat (Eps, Eps)))))
    [<Test>]
    member __.``does not remove nullable trailing subparsers with partial parse trees``() =
        let p = (Cat (Char 'a', Cat ((Eps' (set [[]])), Cat ((Eps' (set [[]])), (Eps' (set [[]]))))))
        parserEqual p (compact p)
    [<Test>]
    member __.``Nothing then something is something``() =
        parserEqual (Char 'a') (compact (Cat (Eps, Char 'a')))
    [<Test>]
    member __.``Nothing then something is something, and compact``() =
        parserEqual (Char 'a') (compact (Cat (Eps, Cat (Eps, Char 'a'))))
    [<Test>]
    member __.``Something then nothing is something``() =
        parserEqual (Char 'a') (compact (Cat (Char 'a', Eps)))
    [<Test>]
    member __.``Something then nothing is something, and compact``() =
        parserEqual (Char 'a') (compact (Cat (Char 'a', Cat (Eps, Eps))))
    [<Test>]
    member __.``Nothing then nothing is nothing``() =
        parserEqual Eps (compact (Cat (Eps, Eps)))
    [<Test>]
    member __.``Something in between nothing is something``() =
        parserEqual (Char 'a') (compact (Cat (Eps, Cat (Char 'a', Eps))))
    [<Test>]
    member __.``compacts sequences of parsers``() =
        parserEqual (Cat (Char 'a', Char 'b')) (compact (Cat (Cat (Eps, Char 'a'), Cat(Eps, Char 'b'))))
    [<Test>]
    member __.``of Star of Eps is Eps``() =
        parserEqual Eps (compact (Star Eps))
    [<Test>]
    member __.``of a Union should return a compact parser``() =
        parserEqual Eps (compact (Union (Eps, Star Empty)))
    [<Test>]
    member __.``of a Union of two identical parser should return that parser, compacted``() =
        parserEqual Eps (compact (Union (Star Empty, Star Empty)))
    [<Test>]
    member __.``Eps then something nullable is that nullable subparser``() =
        parserEqual (Star (Char 'a')) ((compact (Cat (Eps, (Star (Char 'a'))))))
    [<Test>]
    member __.``of a compacted parser is itself``() =
        let compactedParserIsCompact (p: Parser<char>) =
            let cp = compact p
            cp = compact cp
        Check.QuickThrowOnFailure compactedParserIsCompact
    [<Test>]
    member __.``does not alter the accepted language``() =
        let takeSome seq =
            seq
            |> Seq.truncate 1000
        let langUnaltered (p: Parser<char>) =
            let expected = takeSome (generate p)
            let actual = takeSome (generate (compact p))
            let theSame = Seq.zip expected actual |> Seq.map (fun (es,xs) -> Seq.zip es xs |> Seq.map (fun (e, a) -> e = a) |> Seq.fold (&&) true) |> Seq.fold (&&) true
            if not(theSame) then printfn "Expected %A: Was: %A"  expected actual
            theSame
        Check.QuickThrowOnFailure langUnaltered