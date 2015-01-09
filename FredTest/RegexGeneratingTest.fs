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