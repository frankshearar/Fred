module Regex.Test.Nullability

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

// For the purposes of these tests we use a collection of chars or ints. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Nullability``() =
    [<Test>]
    member __.``Empty is not nullable``() =
        Assert.False(nullable Empty)
    [<Test>]
    member __.``Eps is nullable``() =
        let epsIsNullable (t: Set<char list>) =
            nullable (Eps' t)
        Check.QuickThrowOnFailure epsIsNullable
        Assert.True(nullable Eps)
    [<Test>]
    member __.``Char is not nullable``() =
        Assert.False(nullable (Char 'a'))
    [<Test>]
    member __.``Union is nullable if either subparser is nullable``() =
        Assert.False(nullable (Union (Empty, Empty)))
        Assert.True (nullable (Union (Eps,   Empty)))
        Assert.True (nullable (Union (Empty, Eps)))
        Assert.True (nullable (Union (Eps,   Eps)))
    [<Test>]
    member __.``Cat is nullable if both subparsers are nullable``() =
        Assert.False(nullable (Cat (Empty, Empty)))
        Assert.False(nullable (Cat (Eps,   Empty)))
        Assert.False(nullable (Cat (Empty, Eps)))
        Assert.True (nullable (Cat (Eps,   Eps)))
    [<Test>]
    member __.``Star is nullable if repeated parser is not nullable``() =
        Assert.True(nullable (Star (Char 'a')))
    [<Test>]
    member __.``Star is nullable if repeated parser is nullable``() =
        Assert.True(nullable (Star Eps))
