module Regex.Test.Deriving

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

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
    [<Test>]
    member x.``Not is not of derivative of subparser``() =
        let notDistributesOverDerivation p (x: char) =
            d x (Not p) = Not (d x p)
        Check.QuickThrowOnFailure notDistributesOverDerivation

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
            Eps' (set [[x]]) = (dP x (Char x))
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
    [<Test>]
    member x.``Not is not of derivative of subparser``() =
        let notDistributesOverDerivation p (x: char) =
            dP x (Not p) = Not (dP x p)
        Check.QuickThrowOnFailure notDistributesOverDerivation
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
