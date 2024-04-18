# cauldron

> Double, double toil and trouble;
>
> Fire burn and caldron bubble.
>
> Fillet of a fenny snake,
>
> In the caldron boil and bake;

**cauldron** is a library for performing dependency injection. It's an alternative to
manually wiring the constructors for the components ("beans") of your
application. 

It expects the bean constructors to conform to a certain shape.

**cauldron** should be used at the [composition root](https://stackoverflow.com/questions/6277771/what-is-a-composition-root-in-the-context-of-dependency-injection). Bean constructors shouldn't be aware that **cauldron** exists, or depend on its types.

**cauldron** relies on dynamic typing and finds wiring errors at runtime, not compilation time.

# Why you should(n't) use this library

To be honest, you probably shouldn't use this library. I have noticed that using
**cauldron** is actually *more* verbose that manually doing the wiring yourself.
Perhaps it would start to pay for complex beans with many dependencies, but
I'm not sure.

Another possible objection to this library is that wiring errors are detected at
runtime. I don't find that to be a problem though: the wiring happens at the
very beginning of the application, and it's easy to write an unit test for it.

On the plus side, this library lets you render the graph of dependencies between
beans, something which is difficult to do with naive manual wiring.

Another advantage is that you can easily modify an existing web of dependencies,
be it by inserting a new bean, overriding another, or adding a decorator.

# The expected shape of constructors

**cauldron** expects "bean" constructors to have a shape like:

```
makeServer :: Logger -> Repository -> Server
```

Where `Logger`, `Repository` and `Server` are [records-of-functions](https://www.iankduncan.com/articles/2024-01-26-records-of-effects). `Server` is
the component produced by this constructor, and it has `Logger` and `Repository`
as dependencies.

Sometimes constructors are effectful because they must perform some
initialization (for example allocating some `IORef` for the internal `Server`
state). In that case the shape of the constructor becomes something like:

```
makeServer :: Logger -> Repository -> IO Server
```

or even, for constructors which want to ensure that [resources are
deallocated](https://hackage.haskell.org/package/managed) after we are finished using the bean:

```
makeServer :: Logger -> Repository -> Managed Server
```

Having more than one constructor for the same bean type is disallowed. The
wiring is *type-directed*, so there can't be any ambiguity about which bean
constructor to use.

## Registering secondary beans

More complex constructors can return—besides a "primary" bean as seen in the
previous section—one or more "secondary" beans. For example:

```
makeServer :: Logger -> Repository -> (Initializer, Inspector, Server)
```

or 

```
makeServer :: Logger -> Repository -> IO (Initializer, Inspector, Server)
```

These secondary outputs of a constructor, like `Initializer` and `Inspector`,
must have `Monoid` instances. Unlike with the "primary" bean the constructor produces, they
*can* be produced by more than one constructor. Their values will be aggregated
across all the constructors that produce them.

Constructors can depend on the aggregated value of a secondary bean by taking
the bean as a regular argument. Here, `makeDebuggingServer` receives the
`mappend`ed value of all the `Inspector`s produced by other constructors (or
`mempty`, if no constructor produces them):

```
makeDebuggingServer :: Inspector -> IO DebuggingServer
```

Constructors that produce secondary beans require a bit more work from the
**cauldron** user. He must tell the library which is the primary bean and which
are the secondary ones, because it's not detected automatically.

## Decorators

Decorators are like normal constructors, but they're used to *modify* a primary
bean, instead of *producing* it. Because of that, they usually take the bean
they decorate as an argument:

```
makeServerDecorator :: Server -> Server
```

Like normal constructors, decorators can have their own dependencies (other than the
decorated bean), perform effects, and register secondary beans:

```
makeServerDecorator :: Logger -> Server -> IO (Initializer,Server)
```

# Example code

See [this example application](/app/Main.hs) with dummy components.

# Similarities with the [Java Spring framework IoC container](https://docs.spring.io/spring-framework/reference/core/beans.html)

Some features of this library have loose analogues in how Java Spring handles
dependency injection (although of course Spring has many more features).

First, a big *difference*: there's no analogue here of annotations, or classpath
scanning. Beans and decorators must be explicitly registered. 

- Java POJOs are Haskell [records-of-functions](https://www.iankduncan.com/articles/2024-01-26-records-of-effects), where the functions will usually
be closures which encapsulate access to some shared internal state (state like
configuration values, or mutable references). Functions that return
records-of-functions correspond to POJO constructors.

- [@PostConstruct](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/postconstruct-and-predestroy-annotations.html#page-title) roughly corresponds to effectful constructors.

  Although I expect effectful constructors to be used comparatively more in this
 library than in Spring, because here they're required to initialize mutable
 references used by the beans.

- [decorated self-invocations](https://docs.spring.io/spring-framework/reference/core/aop/proxying.html#aop-understanding-aop-proxies) correspond to constructors that
  depend on the same bean that they produce.

  Note that this is different from decorators that depend on the bean they
  modify. The constructor will receive the fully decorated bean "from the
  future" (with the possibility of infinite loops if it makes use of it too
  eagerly). In contrast, a decorator will receive either the bare "undecorated"
  bean, or the in-construction result of applying the decorators that come
  earlier in the decorator sequence.

- [context hierachies](https://docs.spring.io/spring-framework/reference/testing/testcontext-framework/ctx-management/hierarchies.html) correspond to distributing the constructors into various sets organized in parent-child relationships, so that constructors in a child can see the beans of the parent, but not vice-versa. 

- [injecting all the beans that implement a certain interface as a list](https://twitter.com/NiestrojRobert/status/1746808940435042410) roughly corresponds to a constructor that takes a monoidally aggregated "secondary bean" as an argument. 

Some features I'm not yet sure how to mimic:

- [bean scopes](https://docs.spring.io/spring-framework/reference/core/beans/factory-scopes.html), like [request scope](https://docs.spring.io/spring-framework/reference/core/beans/factory-scopes.html#beans-factory-scopes-other-injection). [This Stack Overflow post](https://stackoverflow.com/a/77174979/1364288) gives some information about how they are implemented in Spring.

  The SO post explains that in Spring the injection of request scoped beans into long-lived beans involves thread-local variables. I explored such a technique for Cauldron [here](https://discourse.haskell.org/t/i-got-rid-of-readert-and-now-my-application-is-hanging-by-a-thread/9330).

# See also

- [registry](https://hackage.haskell.org/package/registry) is a more mature and useable library for dependency injection in Haskell. See [this explanatory video](https://www.youtube.com/watch?v=fFCcvsbCrH8).

- [di library for Python](https://python-dependency-injector.ets-labs.org/). [tweet](https://twitter.com/mr_le_fox/status/1754132600459784364).
