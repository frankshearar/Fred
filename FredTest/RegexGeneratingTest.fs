
module Regex.Test.Generating

open Fred
open NUnit.Framework
open Regex
open Regex.Test.Extensions

[<TestFixture>]
type ``Generating``() =
    [<Test>]
    member x.``Empty is empty seq``() =
        seqEqual Seq.empty (generate Empty)
    [<Test>]
    member x.``Eps contains empty input``() =
        seqEqual (Seq.singleton []) (generate Eps)
        seqEqual (Seq.singleton []) (generate (Eps' (set [])))
    [<Test>]
    member x.``Char anything yields that token``() =
        seqEqual (Seq.singleton ['x']) (generate (Char 'x'))
        seqEqual (Seq.singleton [1]) (generate (Char 1))
    [<Test>]
    member x.``Union yields union of subparsers' languages``() =
        let wordCount = 200
        let a = Char 'a' |> Star
        let b = Char 'b' |> Star
        let expected = seq { yield! [[]; ['a']; ['b']; ['a';'a']; ['b';'b']; ['a';'a';'a']; ['b';'b';'b']]}
        seqEqual expected (generate (Union (a,b)) |> Seq.truncate wordCount)
    [<Test>]
    member x.``Cat yields set catenation of subparsers' languages``() =
        let a = Char 'a' |> Star
        let b = Char 'b' |> Star
        let expected = seq { yield! [[]; ['a']; ['b']; ['a';'a']; ['a';'b']; ['b';'b']; ['a';'a';'a'];['a';'a';'b'];['a';'b';'b'];['b';'b';'b'];['a';'a';'b';'b']] }
        seqEqual expected (generate (Cat (a,b)))
    [<Test>]
    member x.``Star yields Kleene closure of subparser's language``() =
        let ab = any ['a';'b']
        let abStar = Star ab
        let expected = seq { yield! [[]; ['a']; ['b']; ['a';'a'];['b';'a'];['a';'b'];['b';'b'];['a';'a';'a']]}
        seqEqual expected (generate abStar)


let rec nfaEqual expected actual =
    Seq.zip expected actual
    |> Seq.iter (fun (ex, ac) -> stateEqual ex ac)
and stateEqual expected actual =
    Assert.That(0 = compare expected actual, (sprintf "Expected %A got %A" expected actual))
    match expected, actual with
    | State (_, _, expectedDs), State (_, _, actualDs) -> nfaEqual expectedDs actualDs

[<TestFixture>]
type ``NFA``() =
    [<Test>]
    member x.``from Empty``() =
        nfaEqual [] (r2n Empty |> List.ofSeq)
    [<Test>]
    member x.``from Eps``() =
        nfaEqual [State (0, '~', [])] (r2n Eps |> List.ofSeq)
    [<Test>]
    member x.``from Eps'``() = // Remember, Eps' are only  built up during derivation.
        nfaEqual [State (0, '~', [])] (r2n (Eps' (set [['a']])) |> List.ofSeq)
    [<Test>]
    member x.``from Char``() =
        let c = 'a'
        nfaEqual [State (1, c, [State (0, '~', [])])] (r2n (Char c) |> List.ofSeq)
    [<Test>]
    member x.``from Cat``() =
        let ab = Cat (Char 'a', Char 'b')
        nfaEqual [State (2, 'a', [State (1, 'b', [State (0, '~', [])])])] (r2n ab)
    [<Test>]
    member x.``from Union``() =
        let ``a|b`` = Union (Char 'a', Char 'b')
        nfaEqual [State (2, 'a', [State (0, '~', [])])
                  State (1, 'b', [State (0, '~', [])])] (r2n ``a|b``)
    [<Test>]
    member x.``from Star``() =
        let ``ab*a`` = [Cat(Cat (Char 'a', Star (Char 'b')), Char 'a')
                        Cat (Char 'a', Cat (Star (Char 'b'), Char 'a'))]
        let rec g0 = State (0, '~', [])
        and g1 = State (1, 'a', [g0])
        and g2 = State (2, 'b', seq { yield g1; yield g2})
        and g3 = State (3, 'a', seq { yield g1; yield g2})
        and g = Seq.singleton g3
        ``ab*a``
        |> List.iter (fun x -> nfaEqual g (r2n x))