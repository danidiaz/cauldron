# cauldron

> Double, double toil and trouble;
> Fire burn and caldron bubble.
> Fillet of a fenny snake,
> In the caldron boil and bake;

**cauldron** is a library for dependency injection. It's an alternative to manually wiring the constructors for the components of your application. 

It expects the component constructors to conform to a certain shape.

**cauldron** should be used at the [composition root](https://stackoverflow.com/questions/6277771/what-is-a-composition-root-in-the-context-of-dependency-injection). Component constructors shouldn't be aware that **cauldron** exists, or depend on its types.

**cauldron** relies on dynamic typing and finds wiring errors at runtime, not compilation time.

# Why you should(n't) use this library

To be honest, you probably shouldn't use this library. I have noticed that using
**cauldron** is actually *more* verbose that manually doing the wiring yourself.
Perhaps it starts to pay for complex components with many dependencies, but I'm
not sure.

Another possible objection to this library is that wiring errors are found at
runtime. I don't find that to be a problem though: the wiring happens at the
very beginning of the application, and it's easy to write an unit test for it.

On the plus side, this library lets you render the graph of dependencies between
components, something which is difficult to do with naive manual wiring.

# The expected shape of constructors

**cauldron** expects component constructors with a shape like:

```
makeServer :: IO (Logger -> Repository -> Server)
```

Where `Logger`, `Repository` and `Server` are records-of-functions. `Server` is
the component produced by this constructor, and it has `Logger` and `Repository`
as dependencies.

Note that there's an `IO` action that returns a function. That action could be
used to allocate some `IORef` for the internal `Server` state, or perhaps for
reading some initial configuration from a file. Note also that the the action
can't make use of the `Logger` and `Repository` dependencies.

In practice, the outer action doesn't need to be `IO`, it can be any other
`Applicative`.

Two or more constructors for the same component type are disallowed. The wiring
is type-directed, so there can't be any ambiguity about what value constructor
to use.

## Monoidal registrations

More complex constructors can register more than one component by returning a tuple:

```
makeServer :: IO (Logger -> Repository -> (Initializer, Inspector, Server))
```

These secondary outputs of a constructor, like `Initializer` and `Inspector`,
must have `Monoid` instances. Unlike with the "primary" result component, they
can be produced by more than one constructor. Their values will be aggregated
across all constructors that produce them.

Constructors can *depend* on these monoidal registrations, by having them as
arguments:

```
makeDebuggingServer :: IO (Inspector -> DebuggingServer)
```

## Decorators

Decorators are special constructors that modify other components. They are
distinguised by returning [`Endo`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Monoid.html#t:Endo)s:

```
makeServerDecorator :: IO (Endo Server)
```

Like normal constructors, decorators can have dependencies, and produce secondary outputs:

```
makeServerDecorator :: IO (Logger -> (Initializer,Endo Server))
```

# See also

- [registry](https://hackage.haskell.org/package/registry) is a more mature and useable library for dependency injection.