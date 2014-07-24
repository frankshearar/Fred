FRED
----

Fred is an experiment in learning F# through implementing [parsing with derivatives](http://matt.might.net/articles/parsing-with-derivatives/).

The initial implementation uses the [standard](http://dl.acm.org/citation.cfm?id=321249) [Brzozowski derivative](http://www.mpi-sws.org/~turon/re-deriv.pdf) of a regular expression.

Later work will implement Matt Might, David Darais and Daniel Spiewak's work on extending derivative parsing to context free grammars.

At the moment the project is not even alpha quality - it is primarily intended as an exercise in learning F# - but I do intend for the library to eventually end up approximating "production grade".

TODO
----
* Finding matches for a Star parser doesn't return the correct results.
* Implement support for extended REs.
* Implement derivatives for context-free grammars

[![Build status on .NET](https://ci.appveyor.com/api/projects/status/8ix7agowa8rrfu1k/branch/master)](https://ci.appveyor.com/project/frankshearar/fred/branch/master)
[![Build Status on Mono](https://secure.travis-ci.org/frankshearar/Fred.png?branch=master)](http://travis-ci.org/frankshearar/Fred)

Licence
-------
Copyright (C) 2014 by Frank Shearar

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.