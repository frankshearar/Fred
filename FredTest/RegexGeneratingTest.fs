module Regex.Test.Generating

open Fred
open FsCheck
open NUnit.Framework
open Regex
open Regex.Test.Extensions

[<TestFixture>]
type ``Drawing NFAs``() = // As a gross side effect, this also tests emptyNfa & add.
    [<Test>]
    member __.``Empty``() =
        let expected = @"digraph {
}
"
        let actual = draw {Edges = Map.empty; Starts = []; Finals = []}
        Assert.AreEqual(expected, actual)
    [<Test>]
    member __.``Single``() =
        let expected = @"digraph {
    0 [label=""'~'""]
}
"
        let actual = emptyNfa
                     |> add {Ident = 0; Token = '~'} None
                     |> addStartState 0
                     |> draw
        Assert.AreEqual(expected, actual)
    [<Test>]
    member __.``ab``() =
        let expected = @"digraph {
    0 [label=""'a'""]
    1 [label=""'b'""]
    0 -> 1
}
"
        let actual = emptyNfa
                     |> add {Ident = 0; Token = 'a'} (Some 1)
                     |> add {Ident = 1; Token = 'b'} None
                     |> addStartState 0
                     |> draw
        Assert.AreEqual(expected, actual)
    [<Test>]
    member __.``ab*a``() =
        let expected = @"digraph {
    0 [label=""'~'""]
    1 [label=""'a'""]
    2 [label=""'b'""]
    3 [label=""'a'""]
    1 -> 0
    2 -> 1
    2 -> 2
    3 -> 1
    3 -> 2
}
"
        let actual =
            emptyNfa
            |> add {Ident = 0; Token = '~'} None
            |> add {Ident = 1; Token = 'a'} (Some 0)
            |> add {Ident = 2; Token = 'b'} (Some 1)
            |> add {Ident = 2; Token = 'b'} (Some 2)
            |> add {Ident = 3; Token = 'a'} (Some 1)
            |> add {Ident = 3; Token = 'a'} (Some 2)
            |> addStartState 3
            |> draw
        Assert.AreEqual(expected, actual)

let optToBool = function
    | Some _ -> true
    | None   -> false

let boolToOpt = function
    | true -> Some 0
    | false -> None

let edgeCount nfa =
    nfa.Edges |> Map.toList |> List.map snd |> Seq.concat |> Seq.length

let assertEdge fromStateIdx toStateIdx nfa =
    nfa.Edges
    |> Map.tryPick (fun from tos -> (from.Ident = fromStateIdx && Seq.exists (fun x -> x = toStateIdx) tos) |> boolToOpt)
    |> optToBool
    |> Assert.That

let assertEdgesOnly (edges: (int * int) list) nfa =
    edges |> List.iter (fun (x, y) -> assertEdge x y nfa)
    let expectedEdgeCount = edgeCount nfa
    let actualEdgeCount = List.length edges
    Assert.AreEqual(expectedEdgeCount, actualEdgeCount)

let example9 =
    emptyNfa
    |> add {Ident = 0; Token = '~'} None
    |> add {Ident = 1; Token = 'a'} (Some 0)
    |> add {Ident = 2; Token = 'b'} (Some 1)
    |> add {Ident = 2; Token = 'b'} (Some 2)
    |> add {Ident = 3; Token = 'a'} (Some 1)
    |> add {Ident = 3; Token = 'a'} (Some 2)
    |> fun nfa -> {nfa with Starts = [3]}

[<TestFixture>]
type ``Transforming to NFA``() =
    [<Test>]
    member __.``Empty is empty``() =
        let nfa = r2n Empty
        draw nfa |> printfn "%s"
        Assert.AreEqual(0, (edgeCount nfa))
    [<Test>]
    member __.``Eps is (nearly) empty``() =
        let nfa = r2n Eps
        draw nfa |> printfn "%s"
        Assert.AreEqual(0, (edgeCount nfa))
    [<Test>]
    member __.``Eps' is (nearly) empty``() =
        let nfa = r2n (Eps' Set.empty)
        draw nfa |> printfn "%s"
        Assert.AreEqual(0, (edgeCount nfa))
    [<Test>]
    member __.``Char NFA has token state``() =
        let nfa = r2n (Char 'a')
        draw nfa |> printfn "%s"
        Assert.That(nfa.Edges |> Map.tryFindKey (fun state _ -> state.Token = 'a') |> optToBool)
        Assert.AreEqual(1, (edgeCount nfa))
        assertEdgesOnly [1,0] nfa
        Assert.That(nfa.Starts |> List.tryFind (fun idx -> idx = 1) |> optToBool, (sprintf "%A" nfa.Starts))
    [<Test>]
    member __.``Cat``() =
        let nfa = r2n (Cat (Char 'a', Char 'b'))
        draw nfa |> printfn "%s"
        assertEdgesOnly [2,1;1,0] nfa
    [<Test>]
    member __.``Union``() =
        let nfa = r2n (Union (Char 'a', Char 'b'))
        draw nfa |> printfn "%s"
        assertEdgesOnly [2,0;1,0] nfa
    [<Test>]
    member __.``Star routes edges to itself``() =
        let nfa = r2n (Star (Char 'a'))
        draw nfa |> printfn "%s"
        Assert.That(nfa.Starts |> List.tryFind (fun idx -> idx = 1) |> optToBool, (sprintf "%A" nfa.Starts))
        assertEdgesOnly [1,0;1,1] nfa
    [<Test>]
    member __.``Star Empty``() =
        let nfa = r2n (Star Empty)
        draw nfa |> printfn "%s"
        Assert.AreEqual(0, (edgeCount nfa))
    [<Test>]
    member __.``Star Eps``() =
        let nfa = r2n (Star Eps)
        draw nfa |> printfn "%s"
        Assert.AreEqual(0, (edgeCount nfa))
    [<Test>]
    member __.``Star (Cat (Eps, Eps))``() =
        let nfa = r2n (Star (Cat (Eps, Eps)))
        draw nfa |> printfn "%s"
        Assert.AreEqual(0, (edgeCount nfa))
    [<Test>]
    member __.``(a|b)*``() =
        let nfa = any ['a';'b'] |> Star |> r2n
        any ['a';'b'] |> r2n |> draw |> printfn "%s"
        draw nfa |> printfn "%s"
        assertEdgesOnly [2,2; 2,1; 1,1; 1,2; 2,0; 1,0] nfa
    [<Test>]
    member __.``Star of deep union ((h(a|e)r)*)``() =
        let ae = Union (Char 'a', Char 'e')
        let haer = Cat (Char 'h', Cat (ae, (Char 'r')))
        let p = Star haer
        p |> dotify |> printfn "%s"
        let nfa = p |> r2n
        nfa |> draw |> printfn "%s"
        assertEdgesOnly [4,3; 4,2; 3,1; 2,1; 1,4; 1,0] nfa
    [<Test>]
    member __.``(ab)*``() =
        let nfa = Cat (Char 'a', Char 'b') |> Star |> r2n
        draw nfa |> printfn "%s"
        assertEdgesOnly [2,1; 1,0; 1,2] nfa
    [<Test>]
    member __.``example 9``() =
        let nfa = example9
        draw nfa |> printfn "%s"
    [<Test>]
    member __.``works on random parsers``() =
        let canDraw (p: Parser<char>) =
            p |> r2n |> draw |> ignore
            true // Getting here = success
        Check.Quick canDraw

[<Timeout(2000)>] // milliseconds
[<TestFixture>]
type ``Generating``() =
    // expectedWords must be in lexicographical order!
    let expectedGen expectedWords p =
        let actualWords = p |> generate |> Seq.truncate (List.length expectedWords) |> List.ofSeq |> List.map List.ofSeq
        listEqual expectedWords actualWords
    // expectedWords must be in lexicographical order!
    // This test asserts that ONLY the expectedWords are generated.
    let expectedGenExact expectedWords p =
        let actualWords = p |> generate |> Seq.truncate (1 + List.length expectedWords) |> List.ofSeq |> List.map List.ofSeq
        Assert.That(List.length expectedWords = List.length actualWords, sprintf "Unexpected word count. Expected: %A Was: %A" expectedWords actualWords)
        listEqual expectedWords actualWords
    [<Test>]
    member __.``Empty is empty seq``() =
        // How do we distinguish between Empty (no accepting paths at all) and
        // Eps (whose accepting paths are all []) ?
        Empty |> generate |> Seq.iter (fun p -> printfn "%A" p)
        expectedGenExact [] Empty
    [<Test>]
    member __.``Star Empty contains empty input``() =
        expectedGenExact [] (Star Empty) // TODO: Should this be [] or [[]] ?
    [<Test>]
    member __.``Star Eps contains empty input``() =
        expectedGenExact [] (Star Eps) // TODO: Should this be [] or [[]] ?
    [<Test>]
    member __.``Eps contains empty input``() =
        expectedGenExact [] Eps             // TODO: Should this be [] or [[]] ?
        expectedGenExact [] (Eps' (set [])) // TODO: Should this be [] or [[]] ?
    [<Test>]
    member __.``Char anything yields that token``() =
        expectedGen [['x']] (Char 'x')
        expectedGen [[1]] (Char 1)
    [<Test>]
    member __.``can enumerate nodes breadth first through an NFA``() =
        let nfa = example9
        let expected = [3; 1; 2; 0; 1; 2; 0; 1; 2]
        let actual = nfa |> bfsi |> Seq.truncate (List.length expected) |> Seq.toList
        listEqual expected actual
    [<Test>]
    member __.``can enumerate breadth first paths through an NFA``() =
        let nfa = example9
        let expected = [
                        [3]
                        [3; 1]
                        [3; 2]
                        [3; 1; 0]
                        [3; 2; 1]
                        [3; 2; 2]
                        [3; 2; 1; 0]
                        [3; 2; 2; 1]
                        [3; 2; 2; 2]
                        [3; 2; 2; 1; 0]
                       ]
                       |> List.map List.rev // It's easier to read the paths from start -> end, but bfsPath returns paths in REVERSE order.
        example9 |> draw |> printfn "%s"
        printfn "%A" expected
        bfsPath [3] nfa |> Seq.truncate 5 |> Seq.iter (fun s -> printfn "%A" s)
        let actual = nfa |> (bfsPath [3]) |> Seq.truncate (Seq.length expected) |> List.ofSeq |> List.map List.ofSeq
        listEqual expected actual
//        bfsPath nfa |> Seq.iter (fun s -> printfn "%A" s)
    [<Test>]
    member __.``Union yields union of subparsers' languages (finite: (a|b)``() =
        let a = Union (Char 'a', Cat (Char 'a', Char 'a'))
        let b = Union (Char 'b', Cat (Char 'b', Char 'b'))
        let expected = [['a']; ['b']; ['a'; 'a']; ['b'; 'b']]
        expectedGen expected (Union (a,b))
    [<Test>]
    member __.``Union yields union of subparsers' languages (infinite: (a*|b*)``() =
        let a = Char 'a' |> Star
        let b = Char 'b' |> Star
        let expected = [[]; ['a']; ['b']; ['a';'a']; ['b';'b']; ['a';'a';'a']; ['b';'b';'b']]
        expectedGen expected (Union (a,b)) // <-- fails because TWO []s are generated!
    [<Test>]
    member __.``Cat yields set catenation of subparsers' languages (a*b*)``() =
        let a = Char 'a' |> Star
        let b = Char 'b' |> Star
        let expected = [[]; ['a']; ['b']; ['a';'a']; ['a';'b']; ['b';'b']; ['a';'a';'a'];['a';'a';'b'];['a';'b';'b'];['b';'b';'b'];['a';'a';'a';'a']]
        expectedGen expected (Cat (a,b))
    [<Test>]
    member __.``Star yields Kleene closure of subparser's language ((a|b)*)``() =
        let ab = any ['a';'b']
        let abStar = Star ab
        abStar |> dotify |> printfn "%s"
        abStar |> r2n |> draw |> printfn "%s"
        let expected = [[]; ['a']; ['b']; ['a';'a'];['a';'b'];['b';'a'];['b';'b'];['a';'a';'a']]
        expectedGen expected abStar
    [<Test>]
    member __.``Simple Cat``() =
        expectedGen [['a'; 'b']] (Cat (Char 'a', Char 'b'))
    [<Test>]
    member __.``Simple Union``() =
        // The tricky part of this test is that a Union parser has TWO start states, not just one!
        let p = Union (Char 'a', Char 'b')
        let expected = [['a']
                        ['b']]
        expectedGen expected p
    [<Test>]
    member __.``example 9``() =
        let ``b*`` = Star (Char 'b')
        let sandwich = Cat (Char 'a', Cat (``b*``, Char 'a'))
        draw (r2n sandwich) |> printfn "%s"
        let expected = [['a'; 'a']
                        ['a'; 'b'; 'a'; ]
                        ['a'; 'b'; 'b'; 'a'; ]
                        ['a'; 'b'; 'b'; 'b'; 'a'; ]]
        expectedGen expected sandwich


//let rec nfaEqual (expected: NFA<'a>) (actual: NFA<'a>) =
//    Seq.zip expected.States actual.States
//    |> Seq.iter (fun (ex, ac) -> stateEqual ex ac)
//and stateEqual expected actual =
//    Assert.That(0 = compare expected actual, (sprintf "Expected %A got %A" expected actual))
//    match expected, actual with
//    | State (_, _, expectedDs), State (_, _, actualDs) -> nfaEqual expectedDs actualDs
//
//[<TestFixture>]
//type ``NFA``() =
//    [<Test>]
//    member __.``from Empty``() =
//        nfaEqual NFA<_>.Empty (r2n Empty)
//    [<Test>]
//    member __.``from Eps``() =
//        nfaEqual (NFA<_>.Simple(State (0, '\000', NFA<_>.Empty))) (r2n Eps)
//    [<Test>]
//    member __.``from Eps'``() = // Remember, Eps' are only  built up during derivation.
//        nfaEqual (NFA<_>.Simple(State (0, '\000', NFA<_>.Empty))) (r2n (Eps' (set [['a']])))
//    [<Test>]
//    member __.``from Char``() =
//        let c = 'a'
//        nfaEqual (NFA<_>.Simple(State (1, c, NFA<_>.Simple(State (0, '\000', NFA<_>.Empty))))) (r2n (Char c))
//    [<Test>]
//    member __.``from Cat``() =
//        let ab = Cat (Char 'a', Char 'b')
//        nfaEqual (NFA<_>.Simple(State (2, 'a', NFA<_>.Simple(State (1, 'b', NFA<_>.Simple(State (0, '\000', NFA<_>.Empty))))))) (r2n ab)
//    [<Test>]
//    member __.``from Union``() =
//        let ``a|b`` = Union (Char 'a', Char 'b')
//        let endState = State (0, '\000', NFA<_>.Empty)
//        nfaEqual (NFA<_>.Simple(State (2, 'a', (new NFA<_>([endState ; (State (1, 'b', (NFA<_>.Simple(endState))))]))))) (r2n ``a|b``)
//    [<Test>]
//    member __.``from Star``() =
//        let ``ab*a`` = [Cat(Cat (Char 'a', Star (Char 'b')), Char 'a')
//                        Cat (Char 'a', Cat (Star (Char 'b'), Char 'a'))]
//        let rec g0 = NFA<_>.Simple(State (0, '\000', NFA<_>.Empty))
//        and g1 = NFA<_>.Simple(State (1, 'a', g0))
//        and g2 = NFA<_>.Simple(State (2, 'b', new NFA<_>(seq { yield! g1.States; yield! g2.States})))
//        and g3 = NFA<_>.Simple(State (3, 'a', new NFA<_>(seq { yield! g1.States; yield! g2.States})))
//        and g = g3
//        ``ab*a``
//        |> List.iter (fun x -> nfaEqual g (r2n x))
//    [<Test>]
//    member __.``from Star #2``() =
//        let ``(a|b)*`` = Star (Union (Char 'a', Char 'b'))
//        let rec g0 = NFA<_>.Simple(State (0, '\000', NFA<_>.Empty))
//        and g1: NFA<_> = NFA<_>.Simple(State (1, 'a', new NFA<_>(seq { yield! g1.States; yield! g2.States})))
//        and g2: NFA<_> = NFA<_>.Simple(State (2, 'b', new NFA<_>(seq { yield! g1.States; yield! g2.States})))
//        and g = new NFA<_>(seq { yield! g1.States; yield! g2.States})
//        let nfa = (r2n ``(a|b)*``)
//        nfaEqual g nfa