module Regex.Test.Extensions

open NUnit.Framework
open Fred.Regex

let collectionDiffMsg e a =
    // "Actual" indented by two spaces because test runners (well, NCrunch)
    // indent the first line by two spaces.
    sprintf "Expected list: %A\n  Actual list:   %A" e a

let listEqual expected actual =
    List.zip expected actual
    |> List.iter (fun (e,a) -> Assert.AreEqual(sprintf "%A" e, sprintf "%A" a, (collectionDiffMsg expected actual)))
    Assert.AreEqual(List.length expected, List.length actual, "List length")

let seqEqual expected actual =
    let limit = 10
    Seq.zip expected actual
    |> Seq.truncate limit
    // This retardedness comes
    // a) from F# not knowing which overload to use and
    // b) sprintf lets the elements stay generic (as opposed to being constrained to object).
    |> Seq.iter (fun (e,a) -> Assert.AreEqual(sprintf "%A" e, sprintf "%A" a, (collectionDiffMsg expected actual)))
    Assert.AreEqual(Seq.isEmpty expected, Seq.isEmpty actual, "Seq length")

let setEqual expected actual =
    Assert.AreEqual((sprintf "%A" expected), (sprintf "%A" actual))

let parserEqual (expected: Parser<'a>) (actual: Parser<'a>) =
    Assert.AreEqual((sprintf "%A" expected), (sprintf "%A" actual))
