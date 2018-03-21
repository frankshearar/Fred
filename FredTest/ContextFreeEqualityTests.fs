module ContextFree.Test.Equality

open Fred.ContextFree
open NUnit.Framework
open Fred.ContextFree

[<TestFixture>]
type ``Equality``() =
    [<Test>]
    member __.``Empty parsers are equal to each other``() =
        Assert.True(empty().Equals(empty()))
    [<Test>]
    member __.``Eps parsers are equal to each other``() =
        Assert.True(eps().Equals(eps()))
    [<Test>]
    member __.``Eps' parsers are equal to each other if their parse trees are equal``() =
        Assert.True(eps'(Set.empty).Equals(eps'(Set.empty)))
        Assert.True(eps'(Set.singleton [1]).Equals(eps'(Set.singleton [1])))

        Assert.False(eps'(Set.singleton [1]).Equals(eps'(Set.singleton [2])))
    [<Test>]
    member __.``Char parsers are equal if their literals are equal``() =
        Assert.True((char 'a').Equals(char 'a'))
        Assert.True((char 22).Equals(char 22))

        Assert.False((char 'a').Equals(char 'b'))
    [<Test>]
    member __.``Cat parsers are equal if their subparsers are equal (in order)``() =
        Assert.True((cat (char 'a') (char 'b')).Equals(cat (char 'a') (char 'b')))
        Assert.True((cat (char 22) (char 23)).Equals(cat (char 22) (char 23)))

        Assert.False((cat (char 'a') (char 'a')).Equals(cat (char 'b') (char 'b')))
        Assert.False((cat (char 'a') (char 'a')).Equals(cat (char 'b') (char 'a')))
        Assert.False((cat (char 'a') (char 'a')).Equals(cat (char 'a') (char 'b')))
    [<Test>]
    member __.``Plus parsers are equal if their subparsers are equal``() =
        Assert.True((plus (char 'a')).Equals(plus (char 'a')))
        Assert.False((plus (char 'a')).Equals(plus (char 'b')))
    [<Test>]
    member __.``Ref parsers are equal if their subparsers are equal``() =
        let selfRefStar x =
            let p1 = union (cat (refer()) (char x)) (eps())
            let suture = p1 |> left |> first
            stitch suture p1 p1

        Assert.True((selfRefStar 'a').Equals(selfRefStar 'a'))
        Assert.False((selfRefStar 'a').Equals(selfRefStar 'b'))
    [<Test>]
    member __.``Star parsers are equal if their subparsers are equal``() =
        Assert.True((star (char 'a')).Equals(star (char 'a')))
        Assert.False((star (char 'a')).Equals(star (char 'b')))
    [<Test>]
    member __.``Union parsers are equal if their subparsers are equal (regardless of order)``() =
        Assert.True((union (char 'a') (char 'b')).Equals(union (char 'a') (char 'b')))
        Assert.True((union (char 22) (char 23)).Equals(union (char 22) (char 23)))
        Assert.True((union (char 'a') (char 'b')).Equals(union (char 'b') (char 'a')))

        Assert.False((union (char 'a') (char 'a')).Equals(union (char 'b') (char 'b')))

