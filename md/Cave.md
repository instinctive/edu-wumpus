# Cave

Resources:

* [Cave.hs](../src/Cave.hs)

We begin with a set of utilities for dealing with the cave layout. Take a look
at the documentation for `Cave`. (You **did** [build the
documentation](../INSTALL.md), didn't you?) This tells you what the expected
behavior of your implementation will be.

Since this is the first file in the exercise, take a look at [how to run the
tests](Testing.md), so you know when you have implemented things correctly.

In sum: implement the `undefined` values, and then run:

    $ cd /path/to/edu-wumpus
    $ stack test

Now I will talk about some things you will find in [Cave.hs](../src/Cave.hs).

## Using base-prelude

    {-# LANGUAGE NoImplicitPrelude #-}
    ...
    import BasePrelude

Throughout this project, I have replaced the standard Haskell prelude with
[base-prelude](https://hackage.haskell.org/package/base-prelude). This
saves us from having to write a ton of `import` statements.

## RecordWildCards

    {-# LANGUAGE RecordWildCards   #-}
    ...
    newtype CaveMap = CaveMap { getCaveMap :: Map Cave [Cave] }
    ...
    rndCave :: MonadRandom m => CaveMap -> m Cave
    rndCave CaveMap {..} = undefined

`RecordWildCards` is a language extension that binds record labels to their
values if we use the `{..}` syntax in the argument to the function. In this
case, we are passing a `CaveMap` to the `rndCave` function. The use of
`CaveMap {..}` in the arguments of the `rndCave` definition means that, within
the body of the definition, `getCaveMap` will be bound to the `Map`.

Here's an example with the extension:

    rndCave CaveMap {..} = ... doSomethingTo getCaveMap

Here's the same example without:

    rndCave c = ... doSomethingTo (getCaveMap c)

It may seem trivial, but when your records have a lot of fields, it can be
very useful.

## MonadRandom

In that same code for `rndCave`:

    rndCave :: MonadRandom m => CaveMap -> m Cave
    rndCave CaveMap {..} = undefined

The `MonadRandom` type constraint says that the body of this function is in
the context of a monad `m` that has the `MonadRandom` constraint. [Look at the
documentation](https://hackage.haskell.org/package/MonadRandom-0.5.1/docs/Control-Monad-Random-Class.html).

The documentation says that, within the context of `MonadRandom`, we have
access to several functions, including `getRandomR`, which you will use in
defining `rndCave`.

    getRandomR :: Random a => (a, a) -> m a

You may wonder, "what is the definition" of `getRandomR`? That needs to be
supplied by whoever defines the *instance* of `MonadRandom` for whatever the
actual monad turns out to be.

In the code for `rndCave`, you don't need to worry about that, because the
typesignature is essentially saying that you promise only to use this function
inside an appropriate context.

But just to jump ahead, we will ultimately use this code inside the `IO`
monad. Look a little further down that same documentation, and you will see
that the `MonadRandom IO` instance has already been defined.

See also this explanation of [Functors, Applicatives, and Monads](Monad.md).
