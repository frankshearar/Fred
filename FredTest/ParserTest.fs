module ParserTest

// NOTE WELL: At the moment this is _very much_ a work in progress. Stuff _does not work_.

open Fred
open NUnit.Framework
open Cfg

let a: Parser<string,string> = Char "a"
let aref = Ref (ref a)
let root = Cat (a, aref)
let cyclicAstar = setRef root aref

[<TestFixture>]
type nullability() =
    [<Test>]
    member this.Empty_is_not_nullable() =
        Assert.False(nullable Empty)
    [<Test>]
    member this.Eps_is_nullable() =
        Assert.That(Eps 1 |> nullable)
    [<Test>]
    member this.Char_is_not_nullable() =
        Assert.False(Char 1 |> nullable)
//    [<Test>]
//    member this.Red_is_nullable() =
//        Assert.That(Char 1 |> Rep |> nullable)
    [<Test>]
    member this.Union_is_nullable_if_left_parser_is() =
        Assert.That(Union (Eps 1, Empty) |> nullable)
    [<Test>]
    member this.Union_is_nullable_if_right_parser_is() =
        Assert.That(Union (Empty, Eps 1) |> nullable)
    [<Test>]
    member this.Union_is_nullable_if_both_parsers_are() =
        Assert.That(Union(Eps 1, Eps 2) |> nullable)
    [<Test>]
    member this.Union_is_not_nullable_if_neither_parser_is() =
        Assert.False(Union (Empty, Empty) |> nullable)
//    [<Test>]
//    member this.Cat_is_nullable_if_both_parsers_are() =
//        Assert.That(Cat (Eps 1, Eps 2) |> nullable)
//    [<Test>]
//    member this.Cat_is_not_nullable_if_only_first_parser_is() =
//        Assert.False(Cat (Empty, Eps 2) |> nullable)
// TODO: Tests showing that we may check recursive/looping parsers for nullability!

let seqEqual (a: 'a list) b =
    (List.zip a b) |> List.mapi (fun i (x, y) -> Assert.True((x = y), (sprintf "%A != %A at index %A" x y i)))

[<TestFixture>]
type empty_parser() =
    [<Test>]
    member this.can_parse_empty_string() =
        Assert.AreEqual([], (parse [] Empty))

[<TestFixture>]
type ``Experiments in Cycles`` () =
    let id = fun x -> x
    [<Test>]
    member x.parse() =
        seqEqual ["a"; "a"] (parse ["a"; "a"] cyclicAstar)
    [<Test>]
    member x.``map empty``() =
        Assert.AreEqual([Empty], (map id Empty))
    [<Test>]
    member x.``map eps``() =
        Assert.AreEqual([Eps []], (map id (Eps [])))
    [<Test>]
    member x.``map cycle``() =
        let parsers = map id cyclicAstar
        match List.ofSeq parsers with
            | first::second::third::[] ->
                Assert.AreSame(cyclicAstar, first)
                Assert.AreSame(aref, second)
                Assert.AreSame(a, third)
            | x -> Assert.Fail(sprintf "Unexpected number of parsers: %A" (List.length x))

[<TestFixture>]
type ``Printing`` () =
    [<Test>]
    member x.empty() =
        Assert.AreEqual("digraph {\n  1 [label=\"empty\"]\n}", print Empty)
    [<Test>]
    member x.eps() =
        Assert.AreEqual("digraph {\n  1 [shape=\"record\" label=\"eps \\\"a\\\"\"]\n}", print (Eps "a"))
    [<Test>]
    member x.ref() =
        Assert.AreEqual("digraph {\n  1 [label=\"ref\"]\n  2 [label=\"empty\"]\n  1 -> 2\n}", print (Ref (ref Empty)))
    [<Test>]
    member x.cat() =
        Assert.AreEqual("digraph {\n  1 [label=\"Cat\"]\n  2 [label=\"empty\"]\n  3 [label=\"empty\"]\n  1 -> 2\n  1 -> 3\n}", print (Cat (Empty, Empty)))
    [<Test>]
    member x.union() =
        Assert.AreEqual("digraph {\n  1 [label=\"Union\"]\n  2 [label=\"empty\"]\n  3 [label=\"empty\"]\n  1 -> 2\n  1 -> 3\n}", print (Union (Empty, Empty)))

[<TestFixture>]
type ``Navigation`` () =
    let p1 = Empty
    let p2 = Char "a"
    let u = Union (p1, p2)
    let c = Cat (p1, p2)
    let r = Ref (ref p1)
    [<Test>]
    member x.left_destructures_union() =
        Assert.AreSame(p1, (left u))
    [<Test>]
    member x.right_destructures_union() =
        Assert.AreSame(p2, (right u))
    [<Test>]
    member x.first_destructures_cat() =
        Assert.AreSame(p1, (first c))
    [<Test>]
    member x.second_destructures_cat() =
        Assert.AreSame(p2, (second c))
    [<Test>]
    member x.deref_structures_refs() =
        Assert.AreSame(p1, (deref r))

[<TestFixture>]
type ``Comparing`` () =
    static let parsers =
        [[Eps "";
        Empty;
        Char "a";
        Union (Empty, Empty);
        Cat (Empty, Empty);
        Ref (ref Empty)]]
    [<Test>]
    member x.``Empty less than everything except Eps`` () =
        Assert.That(Empty > Eps 1)
        Assert.That(Empty > Eps "")
        Assert.That(Empty < Char 1)
        Assert.That(Empty < Union (Empty, Empty))
        Assert.That(Empty < Cat (Empty, Empty))
        Assert.That(Empty < Ref (ref Empty))
    [<Test>]
    member x.``Empty uses compare``() =
        Assert.AreEqual(0, (compare Empty Empty))
    [<Test>]
    member x.``Eps less than everything``() =
        Assert.That(Eps 1 < Empty)
        Assert.That(Eps 1 < Char 1)
        Assert.That(Eps 1 < Union ((Eps 1), (Eps 1)))
        Assert.That(Eps 1 < Cat ((Eps 1), (Eps 1)))
        Assert.That(Eps 1 < Ref (ref (Eps 1)))
    [<Test>]
    member x.``Eps uses compare``() =
        Assert.AreEqual(0, (compare (Eps 1) (Eps 1)))
        Assert.That(Eps 1 > Eps 0)
        Assert.That(Eps 0 < Eps 1)
    [<Test>]
    member x.``Char less than compound parsers1``() =
        Assert.That(Char 1 < Union (Empty, Empty))
    [<Test>]
    member x.``Char less than compound parsers2``() =
        Assert.That(Char 1 < Cat (Empty, Empty))
    [<Test>]
    member x.``Char less than compound parsers3``() =
        Assert.That(Char 1 < Ref (ref Empty))
    [<Test>]
    member x.``Char less than compound parsers``() =
        Assert.That(Char 1 < Union (Empty, Empty))
        Assert.That(Char 1 < Cat (Empty, Empty))
        Assert.That(Char 1 < Ref (ref Empty))
    [<Test>]
    member x.``Char uses compare``() =
        Assert.AreEqual(0, (compare (Char 1) (Char 1)))
        Assert.That(Char 0 < Char 1)
        Assert.That(Char 1 > Char 0)
    [<Test>]
    member x.``Union uses compare``() =
        Assert.AreEqual(0, (compare (Union (Char 1, Char 2)) (Union (Char 1, Char 2))))
        Assert.That(Union (Char 1, Char 2) < Union (Char 2, Char 2))
        Assert.That(Union (Char 1, Char 1) < Union (Char 1, Char 2))
        Assert.That(Union (Char 2, Char 1) > Union (Char 1, Char 1))
        Assert.That(Union (Char 1, Char 2) > Union (Char 1, Char 1))
    [<Test>]
    member x.``Union less than Cat, Ref``() =
        Assert.That(Union (Char 1, Char 1) < Cat (Char 1, Char 1))
        Assert.That(Union (Char 1, Char 1) < Ref (ref (Char 1)))
    [<Test>]
    member x.``Ref greater than everything``() =
        Assert.That(Ref (ref Empty) > Eps 1)
        Assert.That(Ref (ref Empty) > Eps "")
        Assert.That(Ref (ref Empty) > Char 1)
        Assert.That(Ref (ref Empty) > Union (Ref (ref Empty), Ref (ref Empty)))
        Assert.That(Ref (ref Empty) > Cat (Ref (ref Empty), Ref (ref Empty)))
//    [<Test>]
//    member x.``Ref uses compare``() =
//        Assert.AreEqual(0, (compare (Ref (ref (Char 1))) (Ref (ref (Char 1)))))
//        Assert.That(Ref (ref (Char 1)) < Ref (ref (Char 2)))
//        Assert.That(Ref (ref (Char 2)) > Ref (ref (Char 1)))
    [<Test>]
    member x.``Comparison is cycle-safe``() =
        Assert.AreEqual(0, compare cyclicAstar cyclicAstar)

[<TestFixture>]
type ``Equality`` () =
    [<Test>]
    member x.``is cycle-safe``() =
        Assert.That((cyclicAstar = cyclicAstar)) // WTF at (())?!