module Regex.Test.Extensions

open NUnit.Framework

let listEqual expected actual =
    Assert.AreEqual((sprintf "%A" expected), (sprintf "%A" actual))