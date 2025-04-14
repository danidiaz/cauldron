This repository is a Haskell library for performing "dependency injection", by
which I mean type-driven automatic wiring of components, where the components
are functions that return records-of-functions. (The records-of-functions
roughly correspond to objects in object-oriented programming, while the
functions that return records-of-functions correspond to class constructors.)

The library supports agumenting the records-of-functions with "decorators",
which basically correspond to functions that modify an already existing
record-of-functions. "decorators" could be said to correspond to
aspect-oriented-programming.

The library makes heavy use of the "dynamic typing" features of haskell, like
`Data.Typeable` and `Type.Reflection`.

It also makes use of a graph library, unsurprisingly since we have to deal with
graphs of dependencies. Most of the time these graphs will be acyclic (DAGs) but
we also support self-dependencies and full-blown dependency cyles in special
cases.

The logic is distributed across several modules, each focused in one main
concept. I often use explictly qualified imports when functions in one module
are used in another. My expectation is to keep my functions names concise and
relevant for the local types, and rely on qualified imports to resolve
ambiguities.

We use the "tasty" library for the tests. The tests are distributed across a
number of compilation units.

There's also an excutable that gives an example of how to use the library. But
the main focus of this repo is the library itself.