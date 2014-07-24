module RegexTests

open Fred
open NUnit.Framework
open Regex

let listEqual expected actual =
    Assert.AreEqual((sprintf "%A" expected), (sprintf "%A" actual))

// For the purposes of these tests we use a collection of chars. However,
// Regex parsers are generic in their input type!

[<TestFixture>]
type ``Testing``() =
    [<Test>]
    member x.``Empty is not nullable``() =
        Assert.False(nullable Empty)
    [<Test>]
    member x.``Eps is nullable``() =
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
    [<Test>]
    member x.``Empty is empty``() =
        Assert.True(empty Empty)
    [<Test>]
    member x.``Eps is not empty``() =
        Assert.False(empty Eps)
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

[<TestFixture>]
type ``Matching``() =
    [<Test>]
    member x.``Empty``() =
        Assert.False(matches Empty [])
    [<Test>]
    member x.``Eps matches empty input``() =
        Assert.That(matches Eps [])
    [<Test>]
    member x.``Eps doesn't match non-empty input``() =
        Assert.False(matches Eps ['a'])
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
        listEqual ["aa"; "aaa"] (findMatches (Star (Char 'a')) "aabaaa")
