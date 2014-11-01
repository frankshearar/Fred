module Regex.Test.Extensions

open NUnit.Framework
open Fred.Regex

let listEqual expected actual =
    Assert.AreEqual((sprintf "%A" expected), (sprintf "%A" actual))

let seqEqual expected actual =
    Assert.AreEqual((sprintf "%A" expected), (sprintf "%A" actual))

let parserEqual (expected: Parser<'a>) (actual: Parser<'a>) =
    Assert.AreEqual((sprintf "%A" expected), (sprintf "%A" actual))
