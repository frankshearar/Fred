module RegexTests

open Fred
open FsCheck
open NUnit.Framework
open Regex

let listEqual expected actual =
    Assert.AreEqual((sprintf "%A" expected), (sprintf "%A" actual))

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Nullability``() =
    [<Test>]
    member x.``Empty is not nullable``() =
        Assert.False(nullable Empty)
    [<Test>]
    member x.``Eps is nullable``() =
        let epsIsNullable (t: Set<char list>) =
            nullable (Eps' t)
        Check.QuickThrowOnFailure epsIsNullable
        Assert.True(nullable Eps)
    [<Test>]
    member x.``Char is not nullable``() =
        Assert.False(nullable (Char 'a'))
    [<Test>]
    member x.``Union is nullable if either subparser is nullable``() =
        Assert.False(nullable (Union (Empty, Empty)))
        Assert.True (nullable (Union (Eps,   Empty)))
        Assert.True (nullable (Union (Empty, Eps)))
        Assert.True (nullable (Union (Eps,   Eps)))
    [<Test>]
    member x.``Cat is nullable if both subparsers are nullable``() =
        Assert.False(nullable (Cat (Empty, Empty)))
        Assert.False(nullable (Cat (Eps,   Empty)))
        Assert.False(nullable (Cat (Empty, Eps)))
        Assert.True (nullable (Cat (Eps,   Eps)))
    [<Test>]
    member x.``Star is nullable if repeated parser is not nullable``() =
        Assert.True(nullable (Star (Char 'a')))
    [<Test>]
    member x.``Star is nullable if repeated parser is nullable``() =
        Assert.True(nullable (Star Eps))

[<TestFixture>]
type ``Emptiness``() =
    [<Test>]
    member x.``Empty is empty``() =
        Assert.True(empty Empty)
    [<Test>]
    member x.``Eps [] is not empty``() =
        let epsNotEmpty (t: Set<int list>) =
            not (empty (Eps' t))
        Check.QuickThrowOnFailure epsNotEmpty
    [<Test>]
    member x.``Char is not empty``() =
        Assert.False(empty (Char 'a'))
    [<Test>]
    member x.``Union is empty if both subparsers are empty``() =
        Assert.True (empty (Union (Empty, Empty)))
        Assert.False(empty (Union (Empty, Eps)))
        Assert.False(empty (Union (Eps,   Empty)))
        Assert.False(empty (Union (Eps,   Eps)))
    [<Test>]
    member x.``Cat is empty if either subparser is empty``() =
        Assert.True (empty (Cat (Empty, Empty)))
        Assert.True (empty (Cat (Empty, Eps)))
        Assert.True (empty (Cat (Eps,   Empty)))
        Assert.False(empty (Cat (Eps,   Eps)))
    [<Test>]
    member x.``Star is not empty``() =
        Assert.False(empty (Star Eps))
        Assert.False(empty (Star (Char 'a')))
        Assert.False(empty (Star Empty))
    [<Test>]
    member x.``Emptiness implies non-nullability``() =
        let emptyNeverNullable (p: Parser<char>) = // The choice of char is arbitrary: any IComparable would do
            empty p ==> not (nullable p)
        Check.QuickThrowOnFailure emptyNeverNullable
    [<Test>]
    member x.``Nullability implies non-emptiness``() =
        let nullableNeverEmpty (p: Parser<int>) = // The choice of int is arbitrary: any IComparable would do
            nullable p ==> not (empty p)
        Check.QuickThrowOnFailure nullableNeverEmpty

[<TestFixture>]
type ``Deriving``() =
    [<Test>]
    member x.``Empty is Empty``() =
        let emptyIsAlwaysEmpty (x: char) =
            Empty = (d x Empty)
        Check.QuickThrowOnFailure emptyIsAlwaysEmpty
    [<Test>]
    member x.``Eps is Empty``() =
        let epsIsAlwaysEmpty (x: char) y =
            Empty = (d x (Eps' y))
        Check.QuickThrowOnFailure epsIsAlwaysEmpty
    [<Test>]
    member x.``Char is Eps when a match``() =
        let charDerivesToEpsForMatch (x: char) =
            Eps = (d x (Char x))
        Check.QuickThrowOnFailure charDerivesToEpsForMatch
    [<Test>]
    member x.``Char is Empty when not a match``() =
        let charDerivesToEmptyForNonMatch (x: int) y =
            x <> y ==> (Empty = (d x (Char y)))
        Check.QuickThrowOnFailure charDerivesToEmptyForNonMatch
    [<Test>]
    member x.``Cat with non-nullable first is derivative of first, followed by second``() =
        let catWithNonNullablePrefix p1 p2 (x: char) =
            not (nullable p1) ==>
            (Cat (d x p1, p2) = d x (Cat (p1, p2)))
        Check.QuickThrowOnFailure catWithNonNullablePrefix
    [<Test>]
    member x.``Cat with nullable first is either derivative of second, or derivative of first then second``() =
        let catWithNullablePrefix p1 p2 (x: char) =
            nullable p1 ==>
            (Union (d x p2, (Cat (d x p1, p2))) = d x (Cat (p1, p2)))
        Check.QuickThrowOnFailure catWithNullablePrefix
    [<Test>]
    member x.``Union is Union of the derivatives of the subparsers``() =
        let dOfUnionIsUnionOfDs p1 p2 (x: char) =
            Union (d x p1, d x p2) = d x (Union (p1, p2))
        Check.QuickThrowOnFailure dOfUnionIsUnionOfDs
    [<Test>]
    member x.``Star 'peels off' a subparser and derives it``() =
        let dOfStarPeelsOffSubparser p (x: char) =
            Cat (d x p, Star p) = d x (Star p)
        Check.QuickThrowOnFailure dOfStarPeelsOffSubparser

// It's annoying having a separate test suite for d and dP. The two are
// identical except for the derivative of Cat.
// Given Cat a,b where a is nullable,
//
// d  c (Cat a,b) = Union(                         d  c b,  (Cat (d  c a, b))
// dP c (Cat a,b) = Union(Cat (Eps' (parseNull a), dP c b), (Cat (dP c a, b))
//
// That leading Eps' makes all the difference: functionally, it's just
// a no-op, but structurally it makes a big difference.
// We could test for equality between d and dP by testing the generated
// languages, but that's RATHER EXPENSIVE.
[<TestFixture>]
type ``Deriving parse trees``() =
    [<Test>]
    member x.``Empty is Empty``() =
        let emptyIsAlwaysEmpty (x: char) =
            Empty = (dP x Empty)
        Check.QuickThrowOnFailure emptyIsAlwaysEmpty
    [<Test>]
    member x.``Eps is Empty``() =
        let epsIsAlwaysEmpty (x: char) y =
            Empty = (dP x (Eps' y))
        Check.QuickThrowOnFailure epsIsAlwaysEmpty
    [<Test>]
    member x.``Char is Eps' when a match``() =
        let charDerivesToEpsForMatch (x: char) =
            Eps' (Set.singleton [x]) = (dP x (Char x))
        Check.QuickThrowOnFailure charDerivesToEpsForMatch
    [<Test>]
    member x.``Char is Empty when not a match``() =
        let charDerivesToEmptyForNonMatch (x: int) y =
            x <> y ==> (Empty = (dP x (Char y)))
        Check.QuickThrowOnFailure charDerivesToEmptyForNonMatch
    [<Test>]
    member x.``Cat with non-nullable first is derivative of first, followed by second``() =
        let catWithNonNullablePrefix p1 p2 (x: char) =
            not (nullable p1) ==>
            (Cat (dP x p1, p2) = dP x (Cat (p1, p2)))
        Check.QuickThrowOnFailure catWithNonNullablePrefix
    [<Test>]
    member x.``Cat with nullable first is either cat of set of partial parses with derivative of second, or derivative of first then second``() =
        let catWithNullablePrefix p1 p2 (x: char) =
            nullable p1 ==>
            let firstParseTrees = parseNull p1
            ((Union (Cat (Eps' firstParseTrees, dP x p2), (Cat (dP x p1, p2)))) = dP x (Cat (p1, p2)))
        Check.QuickThrowOnFailure catWithNullablePrefix
    [<Test>]
    member x.``Union is Union of the derivatives of the subparsers``() =
        let dOfUnionIsUnionOfDs p1 p2 (x: char) =
            Union (dP x p1, dP x p2) = dP x (Union (p1, p2))
        Check.QuickThrowOnFailure dOfUnionIsUnionOfDs
    [<Test>]
    member x.``Star 'peels off' a subparser and derives it``() =
        let dOfStarPeelsOffSubparser p (x: char) =
            Cat (dP x p, Star p) = dP x (Star p)
        Check.QuickThrowOnFailure dOfStarPeelsOffSubparser
// See the comment in dP's definition for why we can't yet uncomment this test
//    [<Test>]
//    member x.``Except for Char->Eps, both derivations are equal``() =
//        let id = fun x -> x
//        let idmerge = fun x a b -> x
//        let parserToList p =
//            let acc = ref []
//            postfixWalk (fun x -> acc := x::(!acc); x) idmerge p |> ignore
//            !acc
//        let nearlyEqual a b =
//            let xs = parserToList a
//            let ys = parserToList b
//            List.zip xs ys
//            |> List.map (fun (x,y) -> match x,y with
//                                      | Eps, Eps' _ -> true
//                                      | Eps' _, Eps -> true
//                                      | _            -> x = y)
//            |> List.fold (&&) true
//        let sameDerivatives (p: Parser<char>) x =
//            nearlyEqual (d x p) (dP x p)
//        Check.QuickThrowOnFailure sameDerivatives

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
        Assert.That(matches (Eps' (Set.singleton [])) [])
    [<Test>]
    member x.``Eps' [] doesn't match non-empty input``() =
        Assert.False(matches (Eps' (Set.singleton [])) ['a'])
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
        Assert.AreEqual([], (findMatches (Eps' (Set.singleton [])) ""))
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
        equal2 ([],[])    (findSubMatch (Eps' (Set.singleton [])) (List.ofSeq ""))
        equal2 ([],['a']) (findSubMatch (Eps' (Set.singleton [])) (List.ofSeq "a"))
        equal2 ([], ['a'; 'a']) (findSubMatch (Cat (Char 'a', Char 'b')) (List.ofSeq "aa"))
    [<Test>]
    member x.``of a submatch loses no input``() =
        let noInputLost (p: Parser<int>) inputList =
            let matchedPrefix, remainder = findSubMatch p inputList
            (List.append matchedPrefix remainder) = inputList
        Check.QuickThrowOnFailure noInputLost

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
        Assert.AreEqual(Eps' (Set.singleton []), compact (Eps' (Set.singleton [])))
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
        let p = (Cat (Char 'a', Cat ((Eps' (Set.singleton [])), Cat ((Eps' (Set.singleton [])), (Eps' (Set.singleton []))))))
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

[<TestFixture>]
type ``Interleaving of Seqs``() =
    [<Test>]
    member x.``returns empty if both subseqs are empty``() =
        listEqual [] (List.ofSeq (interleave [Seq.empty; Seq.empty]))
    [<Test>]
    member x.``returns left items if right subseq is empty``() =
        interleave [Seq.ofList [1;2;3]; Seq.empty]
        |> List.ofSeq
        |> listEqual [1;2;3]
    [<Test>]
    member x.``returns right items if left subseq is empty``() =
        interleave [Seq.empty; Seq.ofList [1;2;3]]
        |> List.ofSeq
        |> listEqual [1;2;3]
    [<Test>]
    member x.``returns from both seqs if neither are empty``() =
        interleave [Seq.ofList [1;2;3]; Seq.ofList [4;5;6]]
        |> List.ofSeq
        |> listEqual [1;4;2;5;3;6]
    [<Test>]
    member x.``can use infinite sequences``() =
        let constantly v _ = v
        let numberCounts = interleave [Seq.initInfinite (constantly 1); Seq.initInfinite (constantly 2)]
                           |> Seq.take 1000
                           |> Seq.countBy (fun x -> x)
                           |> List.ofSeq
        listEqual [1,500;2,500] numberCounts

    [<Test>]
    member x.``handles empty sequences gracefully``() =
        interleave [Seq.ofList [1;2;3]; Seq.empty; Seq.empty; Seq.ofList [4;5;6]]
        |> List.ofSeq
        |> listEqual [1;4;2;5;3;6]
    [<Test>]
    member x.``returns all elements of all sequences``() =
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
    member x.``returns all combinations of all elements of both sequences``() =
        let cartesianProductHasExpectedLength listA listB = // Geneflect can't generate seqs, so take lists instead.
            let seqA = listA |> Seq.ofList
            let seqB = listB |> Seq.ofList
            Seq.length (allPairs seqA seqB (fun x y -> 0)) = (Seq.length seqA) * (Seq.length seqB)
        Check.QuickThrowOnFailure cartesianProductHasExpectedLength
    [<Test>]
    member x.``runs the function on all pairs``() =
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
type ``testing allTokens``() =
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

[<TestFixture>]
type ``Building parse trees``() =
    [<Test>]
    member x.``from Eps should yield sole parse tree``() =
        let justOneParse (t: char list) =
            Set.singleton t = (parseNull (Eps' (Set.singleton t)))
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