FRED
----

Fred is an experiment in learning F# through implementing [parsing with derivatives](http://matt.might.net/articles/parsing-with-derivatives/).

The initial implementation uses the [standard](http://dl.acm.org/citation.cfm?id=321249) [Brzozowski derivative](http://www.mpi-sws.org/~turon/re-deriv.pdf) of a regular expression.

Later work will implement Matt Might, David Darais and Daniel Spiewak's work on extending derivative parsing to context free grammars.

At the moment the project is largely feature complete for the regular languages, with a C#-friendly wrapper.

Defining parsers
----------------

In C#:
````csharp
// To find all strings "aaa" (or /a{3}/ if you prefer):
RegularParser.Token('a').Count(3)

// /abc(d|e)/
RegularParser.Token('a').Then(RegularParser.Token('b')).Then(RegularParser.Token('c')).Then(RegularParser.Token('d').Or(RegularParser.Token('e')))

// /(ab)*/
(RegularParser.Token('a').Then(RegularParser.Token('b')).Star()

// /a?/
RegularParser.Token('a').AtMost(1)
````

In F#:
````fsharp
// To find all strings "aaa" (or /a{3}/ if you prefer):
rep 3 (Char 'a')    // or:
(Char 'a') |> rep 3


// /abc(d|e)/
Cat (all ['a'; 'b'; 'c'], Union (Char 'd', Char 'e')) // or:
Cat (all (List.ofSeq "abc"), Union (Char 'd', Char 'e')) // because a string is a sequence of char

// /(ab)*/
(all (List.ofSeq "ab")) |> Star

// /a?/
Char 'a' |> atMost 1
````

Finding things
--------------

````csharp
````

````fsharp
let ab = all ['a';'b']
let finder = startFind ab
let someProcessedInput = resumableFind finder "abab"
let someMoreProcessing = resumableFind someProcessedInput "abab"
finishFind someMoreProcessing
````

TODO
----
* Publish on NuGet
* Implement support for extended REs.
* Implement/fix derivatives for context-free grammars

[![Build status on .NET](https://ci.appveyor.com/api/projects/status/8ix7agowa8rrfu1k/branch/master)](https://ci.appveyor.com/project/frankshearar/fred/branch/master)
[![Build Status on Mono](https://secure.travis-ci.org/frankshearar/Fred.png?branch=master)](http://travis-ci.org/frankshearar/Fred)

Licence
-------
Copyright (C) 2014 by Frank Shearar

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.