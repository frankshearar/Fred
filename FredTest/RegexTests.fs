module RegexTests

open Fred
open FsCheck
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
    [<Test>]
    member x.``Emptiness implies non-nullability``() =
        let emptyNeverNullable p =
            empty p ==> not (nullable p)
        Check.QuickThrowOnFailure emptyNeverNullable
    [<Test>]
    member x.``Nullability implies non-emptiness``() =
        let nullableNeverEmpty p =
            nullable p ==> not (empty p)
        Check.QuickThrowOnFailure nullableNeverEmpty

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
        equal2 ([], ['a'; 'a']) (findSubMatch (Cat (Char 'a', Char 'b')) (List.ofSeq "aa"))
    [<Test>]
    member x.``of a submatch loses no input``() =
        let noInputLost p inputList =
            let matchedPrefix, remainder = findSubMatch p inputList
            (List.append matchedPrefix remainder) = inputList
        Check.QuickThrowOnFailure noInputLost

[<TestFixture>]
type ``Compaction``() =
    [<Test>]
    member x.``Empty is compact``() =
        Assert.AreEqual(Empty, compact Empty)
    [<Test>]
    member x.``Eps is compact``() =
        Assert.AreEqual(Eps, compact Eps)
    [<Test>]
    member x.``Char is compact``() =
        Assert.AreEqual(Char 'a', compact (Char 'a'))
    [<Test>]
    member x.``Empty parsers compact to Empty``() =
        Assert.AreEqual(Empty, compact (Union (Empty, Empty)))
//        Assert.AreEqual(Empty, compact (Cat (Empty, Empty)))
    [<Test>]
    member x.``Star Empty = Eps``() =
        Assert.AreEqual(Eps, compact (Star Empty))
    [<Test>]
    member x.``prunes empty subparsers``() =
        Assert.AreEqual(Char 'a', compact (Union (Empty, Char 'a')))
        Assert.AreEqual(Char 'a', compact (Union (Char 'a', Empty)))
    [<Test>]
    member x.``compacts non-empty subparsers``() =
        Assert.AreEqual(Char 'a', compact (Union (Empty, Cat (Eps, Char 'a'))))
        Assert.AreEqual(Char 'a', compact (Union (Cat (Eps, Char 'a'), Empty)))
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
    member x.``Something in between nothing is something``() =
        Assert.AreEqual(Char 'a', compact (Cat (Eps, Cat (Char 'a', Eps))))
    [<Test>]
    member x.``compacts sequences of parsers``() =
        Assert.AreEqual(Cat (Char 'a', Char 'b'), compact (Cat (Cat (Eps, Char 'a'), Cat(Eps, Char 'b'))))
    [<Test>]
    member x.``Star of nothing is Eps``() =
        Assert.AreEqual(Eps, compact (Star Empty))
    [<Test>]
    member x.``of a Union should return a compact parser``() =
        Assert.AreEqual(Eps, compact (Union (Eps, Star Empty)))
    [<Test>]
    member x.``Eps then something nullable is that nullable subparser``() =
        Assert.AreEqual((Star (Char 'a')), (compact (d 'a' (Star (Char 'a')))))
    [<Test>]
    member x.``of a compacted parser is itself``() =
        let compactedParserIsCompact p =
            compact p = compact (compact p)
        Check.QuickThrowOnFailure compactedParserIsCompact
//    [<Test>]
//    member x.``does not alter the accepted language``() =
//        let takeSome seq =
//            seq
//            |> Seq.truncate 100
//            |> List.ofSeq
//        let langUnaltered p =
//            takeSome (generate p) = takeSome (generate (compact p))
//        Check.QuickThrowOnFailure langUnaltered

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