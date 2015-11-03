module Regex.Test.Emptiness

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Emptiness``() =
    [<Test>]
    member __.``Empty is empty``() =
        Assert.True(empty Empty)
    [<Test>]
    member __.``Eps [] is not empty``() =
        let epsNotEmpty (t: Set<int list>) =
            not (empty (Eps' t))
        Check.QuickThrowOnFailure epsNotEmpty
    [<Test>]
    member __.``Char is not empty``() =
        Assert.False(empty (Char 'a'))
    [<Test>]
    member __.``Union is empty if both subparsers are empty``() =
        Assert.True (empty (Union (Empty, Empty)))
        Assert.False(empty (Union (Empty, Eps)))
        Assert.False(empty (Union (Eps,   Empty)))
        Assert.False(empty (Union (Eps,   Eps)))
    [<Test>]
    member __.``Cat is empty if either subparser is empty``() =
        Assert.True (empty (Cat (Empty, Empty)))
        Assert.True (empty (Cat (Empty, Eps)))
        Assert.True (empty (Cat (Eps,   Empty)))
        Assert.False(empty (Cat (Eps,   Eps)))
    [<Test>]
    member __.``Star is not empty``() =
        Assert.False(empty (Star Eps))
        Assert.False(empty (Star (Char 'a')))
        Assert.False(empty (Star Empty))
    [<Test>]
    member __.``Emptiness implies non-nullability``() =
        let emptyNeverNullable (p: Parser<char>) = // The choice of char is arbitrary: any IComparable would do
            empty p ==> not (nullable p)
        Check.QuickThrowOnFailure emptyNeverNullable
    [<Test>]
    member __.``Nullability implies non-emptiness``() =
        let nullableNeverEmpty (p: Parser<int>) = // The choice of int is arbitrary: any IComparable would do
            nullable p ==> not (empty p)
        Check.QuickThrowOnFailure nullableNeverEmpty