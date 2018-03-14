module Test.SeqExtensions

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Interleaving of Seqs``() =
    [<Test>]
    member __.``returns empty if both subseqs are empty``() =
        listEqual [] (List.ofSeq (interleave [Seq.empty; Seq.empty]))
    [<Test>]
    member __.``returns left items if right subseq is empty``() =
        interleave [Seq.ofList [1;2;3]; Seq.empty]
        |> List.ofSeq
        |> listEqual [1;2;3]
    [<Test>]
    member __.``returns right items if left subseq is empty``() =
        interleave [Seq.empty; Seq.ofList [1;2;3]]
        |> List.ofSeq
        |> listEqual [1;2;3]
    [<Test>]
    member __.``returns from both seqs if neither are empty``() =
        interleave [Seq.ofList [1;2;3]; Seq.ofList [4;5;6]]
        |> List.ofSeq
        |> listEqual [1;4;2;5;3;6]
    [<Test>]
    member __.``can use infinite sequences``() =
        let constantly v _ = v
        let numberCounts = interleave [Seq.initInfinite (constantly 1); Seq.initInfinite (constantly 2)]
                           |> Seq.take 1000
                           |> Seq.countBy (fun x -> x)
                           |> List.ofSeq
        listEqual [1,500;2,500] numberCounts

    [<Test>]
    member __.``handles empty sequences gracefully``() =
        interleave [Seq.ofList [1;2;3]; Seq.empty; Seq.empty; Seq.ofList [4;5;6]]
        |> List.ofSeq
        |> listEqual [1;4;2;5;3;6]
    [<Test>]
    member __.``returns all elements of all sequences``() =
        let allElemsReturned (lists: int list list) = // Why not int seq list? FsCheck chokes on generating seqs. See http://fscheck.codeplex.com/workitem/16785
            let left =
                lists
                |> List.map Seq.ofList
                |> interleave
                |> Seq.length
            let right =
                lists
                |> List.map List.length
                |> List.fold (+) 0
            left = right
        Check.QuickThrowOnFailure allElemsReturned

[<TestFixture>]
type ``Taking the product of Seqs``() =
    [<Test>]
    member __.``when both are empty, is Seq.empty``() =
        Assert.AreEqual(Seq.empty, (allPairs Seq.empty Seq.empty (+)))
    [<Test>]
    member __.``when first is empty, is the second``() =
        Assert.AreEqual([1;2;3], (allPairs Seq.empty [1;2;3] (+)))
    [<Test>]
    member __.``when second is empty, is the first``() =
        Assert.AreEqual([1;2;3], (allPairs [1;2;3] Seq.empty (+)))
    [<Test>]
    member __.``returns all combinations of all elements of both non-empty sequences``() =
        let cartesianProductHasExpectedLength listA listB = // Geneflect can't generate seqs, so take lists instead.
            (not (List.isEmpty listA) && not (List.isEmpty listB)) ==>
            let seqA = listA |> Seq.ofList
            let seqB = listB |> Seq.ofList
            Seq.length (allPairs seqA seqB (fun x y -> 0)) = (Seq.length seqA) * (Seq.length seqB)
        Check.QuickThrowOnFailure cartesianProductHasExpectedLength
    [<Test>]
    member __.``returns the non-empty sequence when the other sequence is empty``() =
        listEqual [1;2;3] (allPairs [1;2;3] [] (+))
        listEqual [1;2;3] (allPairs [] [1;2;3] (+))
//        let cartProdIsNonEmptySeq listA listB =
//            (List.isEmpty listA || List.isEmpty listB) ==>
//                let seqA = listA |> Seq.ofList
//                let seqB = listB |> Seq.ofList
//                Seq.length (allPairs seqA seqB List.append) = (Seq.length seqA) + (Seq.length seqB) // empty = 0 length = additive identity
//        Check.QuickThrowOnFailure cartProdIsNonEmptySeq
    [<Test>]
    member __.``runs the function on all pairs``() =
        let functionApplied (listA: int list) listB =
            (not (List.isEmpty listA) && not (List.isEmpty listB)) ==>
            let seqA = listA |> Seq.ofList
            let seqB = listB |> Seq.ofList
            let f = fun x y -> x + y
            // After turning the lists into seqs we need to turn them BACK into lists so we can compare them value-wise.
            let actual = allPairs seqA seqB f |> List.ofSeq
            let expected = seq { for a in listA do
                                     for b in listB do
                                         yield f a b } |> List.ofSeq
            expected = actual
        Check.QuickThrowOnFailure functionApplied

[<TestFixture>]
type ``Testing exactlyEqual``() =
    [<Test>]
    member __.``returns true for singleton seq with expected value``() =
        let just1 = seq {yield 1}
        Assert.That(exactlyEqual just1 1)
    [<Test>]
    member __.``returns false for singleton seq with unexpected value``() =
        let just1 = seq {yield 1}
        Assert.False(exactlyEqual just1 2)
        let just2 = seq {yield 2}
        Assert.False(exactlyEqual just2 1)
    [<Test>]
    member __.``returns false for empty sequences``() =
        let alwaysFalse value =
            false = exactlyEqual Seq.empty value
        Check.QuickThrowOnFailure alwaysFalse
    [<Test>]
    member __.``returns false for multi-item sequences``() =
        let alwaysFalse value =
            let twoVals = seq { yield 1; yield 2}
            false = exactlyEqual twoVals value
        Check.QuickThrowOnFailure alwaysFalse
    [<Test>]
    member __.``returns false for two-item sequences (first)``() =
        let alwaysFalseForFirstVal pair =
            let s = match pair with
                    | a, b -> seq {yield a; yield b}
            false = exactlyEqual s (fst pair)
        Check.QuickThrowOnFailure alwaysFalseForFirstVal
    [<Test>]
    member __.``returns false for two-item sequences (second)``() =
        let alwaysFalseForSecondVal pair =
            let s = match pair with
                    | a, b -> seq {yield a; yield b}
            false = exactlyEqual s (snd pair)
        Check.QuickThrowOnFailure alwaysFalseForSecondVal