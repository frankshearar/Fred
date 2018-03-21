module ContextFree.Test.Deriving

open Fred
open FsCheck
open NUnit.Framework
open ContextFree

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Deriving``() =
    [<Test>]
    member __.``Empty is Empty``() =
        let emptyIsAlwaysEmpty (x: char) =
            empty() = (d x (empty()))
        Check.QuickThrowOnFailure emptyIsAlwaysEmpty
    [<Test>]
    member __.``Eps is Empty``() =
        let epsIsAlwaysEmpty (x: char) y =
            empty() = (d x (eps' y))
        Check.QuickThrowOnFailure epsIsAlwaysEmpty
    [<Test>]
    member __.``Char is Eps when a match``() =
        let charDerivesToEpsForMatch (x: char) =
            eps() = (d x (char x))
        Check.QuickThrowOnFailure charDerivesToEpsForMatch
    [<Test>]
    member __.``Char is Empty when not a match``() =
        let charDerivesToEmptyForNonMatch (x: int) y =
            x <> y ==> (empty() = (d x (char y)))
        Check.QuickThrowOnFailure charDerivesToEmptyForNonMatch
    [<Test>]
    member __.``Cat with non-nullable first is derivative of first, followed by second``() =
        let catWithNonNullablePrefix p1 p2 (x: char) =
            not (nullable p1) ==>
            (cat (d x p1) p2 = d x (cat p1 p2))
        Check.QuickThrowOnFailure catWithNonNullablePrefix
    [<Test>]
    member __.``Cat with nullable first is either derivative of second, or derivative of first then second``() =
        let catWithNullablePrefix p1 p2 (x: char) =
            nullable p1 ==>
            ((union (d x p2) (cat (d x p1) p2)) = (d x (cat p1 p2)))
        Check.QuickThrowOnFailure catWithNullablePrefix
    [<Test>]
    member __.``Union is Union of the derivatives of the subparsers``() =
        let dOfUnionIsUnionOfDs p1 p2 (x: char) =
            union (d x p1) (d x p2) = d x (union p1 p2)
        Check.QuickThrowOnFailure dOfUnionIsUnionOfDs
    [<Test>]
    member __.``Star 'peels off' a subparser and derives it``() =
        let dOfStarPeelsOffSubparser p (x: char) =
            cat (d x p) (star p) = d x (star p)
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
    member __.``Empty is Empty``() =
        let emptyIsAlwaysEmpty (x: char) =
            empty() = (dP x (empty()))
        Check.QuickThrowOnFailure emptyIsAlwaysEmpty
    [<Test>]
    member __.``Eps is Empty``() =
        let epsIsAlwaysEmpty (x: char) y =
            empty() = (dP x (eps' y))
        Check.QuickThrowOnFailure epsIsAlwaysEmpty
    [<Test>]
    member __.``Char is Eps' when a match``() =
        let charDerivesToEpsForMatch (x: char) =
            eps' (set [[x]]) = (dP x (char x))
        Check.QuickThrowOnFailure charDerivesToEpsForMatch
    [<Test>]
    member __.``Char is Empty when not a match``() =
        let charDerivesToEmptyForNonMatch (x: int) y =
            x <> y ==> (empty() = (dP x (char y)))
        Check.QuickThrowOnFailure charDerivesToEmptyForNonMatch
    [<Test>]
    member __.``Cat with non-nullable first is derivative of first, followed by second``() =
        let catWithNonNullablePrefix p1 p2 (x: char) =
            not (nullable p1) ==>
            (cat (dP x p1) p2 = dP x (cat p1 p2))
        Check.QuickThrowOnFailure catWithNonNullablePrefix
    [<Test>]
    member __.``Cat with nullable first is either cat of set of partial parses with derivative of second, or derivative of first then second``() =
        let catWithNullablePrefix p1 p2 (x: char) =
            nullable p1 ==>
            let firstParseTrees = parseNull p1
            ((union (cat (eps' firstParseTrees) (dP x p2)) (cat (dP x p1) p2)) = dP x (cat p1 p2))
        Check.QuickThrowOnFailure catWithNullablePrefix
    [<Test>]
    member __.``Union is Union of the derivatives of the subparsers``() =
        let dOfUnionIsUnionOfDs p1 p2 (x: char) =
            (union (dP x p1) (dP x p2) = dP x (union p1 p2))
        Check.QuickThrowOnFailure dOfUnionIsUnionOfDs
    [<Test>]
    member __.``Star 'peels off' a subparser and derives it``() =
        let dOfStarPeelsOffSubparser p (x: char) =
            cat (dP x p) (star p) = dP x (star p)
        Check.QuickThrowOnFailure dOfStarPeelsOffSubparser
// See the comment in dP's definition for why we can't yet uncomment this test
//    [<Test>]
//    member __.``Except for Char->Eps, both derivations are equal``() =
//        let id = fun x -> x
//        let parserToList p =
//            let acc = ref []
//            map (fun x -> acc := x::(!acc); x) p |> ignore
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
