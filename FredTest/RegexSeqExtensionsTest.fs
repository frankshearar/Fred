module Regex.Test.SeqExtensions

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Testing comp``() =
    [<Test>]
    member x.``with integer comparison works``() =
        Assert.That(match 1, 2 with
                    | LT -> true
                    | _  -> false)
        Assert.That(match 1, 1 with
                    | EQ -> true
                    | _  -> false)
        Assert.That(match 2, 1 with
                    | GT -> true
                    | _  -> false)

[<TestFixture>]
type ``LOLs (length ordered lists)``() =
    [<Test>]
    member x.``are EQ if of same length (and heads of lists are EQ)``() =
        Assert.AreEqual(0, (compare (LOL.over []) (LOL.over [])))
        Assert.AreEqual(0, (compare (LOL.over [1]) (LOL.over [1])))
        Assert.AreEqual(0, (compare (LOL.over [1;1]) (LOL.over [1;1])))
    [<Test>]
    member x.``are LT if shorter than another list``() =
        Assert.AreEqual(-1, (compare [] [1]))
    [<Test>]
    member x.``are LT if first element of same length list is LT``() =
        Assert.AreEqual(-1, (compare [0] [1]))
    [<Test>]
    member x.``are LT if first element of same length list is GT``() =
        Assert.AreEqual(1, (compare [1] [0]))
    [<Test>]
    member x.``are GT if longer than another list``() =
        Assert.AreEqual(1, (compare [1] []))

[<TestFixture>]
type ``Ordered merge``() =
    [<Test>]
    member x.``of two empty seqs is empty``() =
        seqEqual Seq.empty (Seq.empty |/ Seq.empty)
    [<Test>]
    member x.``of empty with seq is seq``() =
        let notEmpty = seq {yield 1; yield 2}
        seqEqual notEmpty (Seq.empty |/ notEmpty)
    [<Test>]
    member x.``of seq with empty is seq``() =
        let notEmpty = seq {yield 1; yield 2}
        seqEqual notEmpty (notEmpty |/ Seq.empty)
    [<Test>]
    member x.``is ordered``() =
        let a = seq {yield 0; yield 1; yield 3}
        let b = seq {yield 1; yield 2}
        seqEqual (seq { yield 0; yield 1; yield 2; yield 3}) (a |/ b)
        seqEqual (seq { yield 0; yield 1; yield 2; yield 3}) (b |/ a)

[<TestFixture>]
type ``xprod (cross product of sequences)``() =
    [<Test>]
    member x.``when both are empty, is Seq.empty``() =
        Assert.AreEqual(Seq.empty, (xprod (+) Seq.empty Seq.empty))
    [<Test>]
    member x.``when first is empty, is empty``() =
        seqEqual Seq.empty (xprod (+) Seq.empty [1;2;3])
    [<Test>]
    member x.``when second is empty, is empty``() =
        seqEqual Seq.empty (xprod (+) [1;2;3] Seq.empty)
    [<Test>]
    member x.``returns all combinations of all elements of both non-empty sequences``() =
        listEqual [1,0; 0,0; 0,1; 1,1] (xprod (fun x y -> x,y) [1;0] [0;1] |> List.ofSeq)
    [<Test>]
    member x.``runs the function on all pairs``() =
        listEqual [8;6;12;14] (xprod (+) [6;4] [2;8] |> List.ofSeq)

[<TestFixture>]
type ``Testing exactlyEqual``() =
    [<Test>]
    member x.``returns true for singleton seq with expected value``() =
        let just1 = seq {yield 1}
        Assert.That(exactlyEqual just1 1)
    [<Test>]
    member x.``returns false for singleton seq with unexpected value``() =
        let just1 = seq {yield 1}
        Assert.False(exactlyEqual just1 2)
        let just2 = seq {yield 2}
        Assert.False(exactlyEqual just2 1)
    [<Test>]
    member x.``returns false for empty sequences``() =
        let alwaysFalse value =
            false = exactlyEqual Seq.empty value
        Check.QuickThrowOnFailure alwaysFalse
    [<Test>]
    member x.``returns false for multi-item sequences``() =
        let alwaysFalse value =
            let twoVals = seq { yield 1; yield 2}
            false = exactlyEqual twoVals value
        Check.QuickThrowOnFailure alwaysFalse
    [<Test>]
    member x.``returns false for two-item sequences (first)``() =
        let alwaysFalseForFirstVal pair =
            let s = match pair with
                    | a, b -> seq {yield a; yield b}
            false = exactlyEqual s (fst pair)
        Check.QuickThrowOnFailure alwaysFalseForFirstVal
    [<Test>]
    member x.``returns false for two-item sequences (second)``() =
        let alwaysFalseForSecondVal pair =
            let s = match pair with
                    | a, b -> seq {yield a; yield b}
            false = exactlyEqual s (snd pair)
        Check.QuickThrowOnFailure alwaysFalseForSecondVal