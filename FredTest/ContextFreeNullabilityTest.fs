module ContextFree.Test.Nullability

open Fred
open FsCheck
open NUnit.Framework
open ContextFree

[<TestFixture>]
type ``Nullability``() =
    [<Test>]
    member __.``empty() is not nullable``() =
        Assert.False(nullable (empty()))
    [<Test>]
    member __.``eps() is nullable``() =
        let epsIsNullable (t: Set<char list>) =
            nullable (eps' t)
        Check.QuickThrowOnFailure epsIsNullable
        Assert.True(nullable (eps()))
    [<Test>]
    member __.``Char is not nullable``() =
        Assert.False(nullable (char 'a'))
    [<Test>]
    member __.``self-referential parsers can have their nullability computed``() =
        let p1 = union (cat (refer()) (char 'a')) (eps())
        let suture = p1 |> left |> first
        let p = stitch suture p1 p1
        printfn "%s" (toDot p)
        Assert.True(nullable p)
    [<Test>]
    member __.``Union is nullable if either subparser is nullable``() =
        Assert.False(nullable (union (empty()) (empty())))
        Assert.True (nullable (union (eps())   (empty())))
        Assert.True (nullable (union (empty()) (eps())))
        Assert.True (nullable (union (eps())   (eps())))
    [<Test>]
    member __.``Cat is nullable if both subparsers are nullable``() =
        Assert.False(nullable (cat (empty()) (empty())))
        Assert.False(nullable (cat (eps())   (empty())))
        Assert.False(nullable (cat (empty()) (eps())))
        Assert.True (nullable (cat (eps())   (eps())))
    [<Test>]
    member __.``Plus is not nullable if repeated parser is not nullable``() =
        Assert.False(nullable (plus (char 'a')))
    [<Test>]
    member __.``Plus is nullable if repeated parser is nullable``() =
        Assert.True(nullable (plus (eps())))
    [<Test>]
    member __.``Star is nullable if repeated parser is not nullable``() =
        Assert.True(nullable (star (char 'a')))
    [<Test>]
    member __.``Star is nullable if repeated parser is nullable``() =
        Assert.True(nullable (star (eps())))
