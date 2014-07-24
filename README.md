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