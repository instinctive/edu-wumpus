# Functor, Applicative, and Monad

Let's gather the class descriptions from
[Data.Functor](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Functor.html),
[Control.Applicative](https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Applicative.html),
and
[Control.Monad](https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Monad.html)
in one place.

    class Functor f where
      (<$>) :: (a -> b) -> f a -> f b      -- operator version of fmap
    
    class Functor f => Applicative f where
      (<*>) :: f (a -> b) -> f a -> f b
      pure :: a -> f a
    
    class Applicative f => Monad f where
      (=<<) :: (a -> f b) -> f a -> f b    -- flipped bind operator
      return :: a -> f a

The meaning of `class Foo x => Bar x where ...` is that every `Bar` is a
`Foo`. So every `Monad` is an `Applicative`, and every `Applicative` is a
`Functor`.

For pedagogy, I am using the operator version of `fmap`, and the flipped
version of the usual "bind" operator. These are equivalent:

    (<$>) = fmap
    (=<<) = flip (>>=)

This is just to use the most similar versions of all these operators. In
actual code, you are free to use whichever version makes sense. For example:

    getCaps :: IO [String]
    getCaps = fmap toUpper . words <$> getLine

This hypothetical code fragment uses both versions of `fmap`,
because it seems convenient to do so.

## A first look

Let's ignore `pure` and `return`, and jump straight to the operators:

    ( $ ) ::   (a ->   b) ->   a ->   b  -- function application
    (<$>) ::   (a ->   b) -> f a -> f b  -- Functor f
    (<*>) :: f (a ->   b) -> f a -> f b  -- Applicative f
    (=<<) ::   (a -> f b) -> f a -> f b  -- Monad f

Look at these types. They are all the same as good old *function application*,
except that there is this `f` type hanging around.

## What is this `f`?

Various tutorials will tell you it is a burrito (joking), or use some other
metaphor.  I think of it as a *context* or a *structure* of a computation. The
quintessential example is `Maybe`.

    data Maybe a = Nothing | Just a

Notice that this `f` is the *type*, `Maybe`, not the *constructor*, e.g., `f`
is *not* `Just`. Let's subsitute `Maybe` into our types:

    ( $ ) ::       (a -> b)       ->       a ->       b  -- function application
    (<$>) ::       (a -> b)       -> Maybe a -> Maybe b  -- Functor Maybe
    (<*>) :: Maybe (a -> b)       -> Maybe a -> Maybe b  -- Applicative Maybe
    (=<<) ::       (a -> Maybe b) -> Maybe a -> Maybe b  -- Monad Maybe

Let's try some examples:

    ( (+3)       $       4 ) ==      7
    ( (+3)      <$> Just 4 ) == Just 7
    ( Just (+3) <*> Just 4 ) == Just 7
    ( Just.(+3) =<< Just 4 ) == Just 7

Each of these expressions is `True`.

The preceeding excerpt was a collection of *types*. This is a collection of
*values* of the corresponding types:

    Just 4 :: Maybe Int

Okay, so all the `Functor`, `Applicative`, and `Monad` fuss is about these `f`
contexts. It's pretty obvious how you construct a value in the `Maybe` context:
you can just wrap it in a `Just`. But what about `IO`? I hear that's a `Monad`,
and there's no such thing as an `IO` constructor...? Read on!

## Injecting values into contexts

Now for `pure` and `return`:

    pure   :: a -> f a    -- Applicative
    return :: a -> f a    -- Monad

Not only do these two functions have the same type signature, they are in fact
the *same* function. Remember above where we said that every `Monad` was an
`Applicative`, and thus also a `Functor`?  [That wasn't always
true](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal). So there
were two functions, which are now the same, and both names live on.

What does this function do? It injects a value into one of those `f` contexts:

    (      (+3)  $       4 ) ==      7
    (      (+3) <$> pure 4 ) == Just 7
    ( pure (+3) <*> pure 4 ) == Just 7
    ( pure.(+3) =<< pure 4 ) == Just 7

Each of these expressions is also `True`. Compare with the preceeding example.
We replaced the `Just`s with `pure`s. Yet somehow it's the same result! What
happened?

Because of the `Just 7` on the right-hand side of the equality, the computation
knows that it is in a `Maybe` context. In other words, `pure 4` must be of type
`Maybe Int`, because the left-hand side type must match the right-hand side
type. So `pure` "injects" the `4` into that context, resulting in `Just 4`.

Now, this might seem like magic. Why is it `Just 4` and not something else?
The answer is that someone had to create an `Applicative instance` for `Maybe`,
in which these `class` operations were defined:

    instance Applicative Maybe where
        pure = Just

You can hunt this actual line of code down yourself!

So, how do we inject a value into the `IO` monad, when there's no `IO`
constructor?

    pure 4 :: IO Int

Easy!

## More about context

Tutorials often start with `Maybe`. Let's branch out into other contexts:

    instance             Functor     ((,) a) where
    instance Monoid a => Applicative ((,) a) where
    instance Monoid a => Monad       ((,) a) where

Two-element tuples `(a,b)` are constructed from two types. These declarations
say that a two-element tuple is a `Functor`, `Applicative`, or `Monad` in its
*second* type.

Here's an example.

    (         length   $       "ok!"  ) ==      3
    (         length  <$> ("b","ok!") ) == ("b",3)
    ( ("",    length) <*> ("b","ok!") ) == ("b",3)
    ( ((,)"").length  =<< ("b","ok!") ) == ("b",3)

Notice that the first element of the tuple didn't change. What happens if we
change those empty strings in the applicative and monad cases?

    ( ("a",    length) <*> ("b","ok!") ) == ("ab",3)
    ( ((,)"a").length  =<< ("b","ok!") ) == ("ba",3)

Whoa! The first element *did* change! Wat!

The issue here is that there is information in the context that needs to be
preserved. The `Monoid a` constraint on the `Applicative` and `Monad` instance
definitions says that this information should be combined "monoidally". In the
case of `String`s, this means appending them together.

(Notice the order is different for the two cases. Also recall we are using the
reversed "bind" operator.)

## The structure of a computation

Let's go back to our types:

    ( $ ) ::   (a ->   b) ->   a ->   b  -- function application
    (<$>) ::   (a ->   b) -> f a -> f b  -- Functor f
    (<*>) :: f (a ->   b) -> f a -> f b  -- Applicative f
    (=<<) ::   (a -> f b) -> f a -> f b  -- Monad f

The extra power of `Monad` comes from the fact that the `f` context is on the
right-hand side of the function arrow. That means a monadic function can
provide whatever context it wants (of type `f`) as its output. Let me say that
again:

> A monadic function can provide whatever context it wants (of type `f`) as its
> output.

This is YUGE.

    fate density :: Float -> IO ()
    fate density
        | density > 1 = out "closed"
        | density < 1 = out "open"
        | otherwise   = out "unknown"
      where out = putStrLn.("The geometry of space is " ++)

Each of the three clauses in this code are of type `IO ()`, but they are all
*different*. The `fate` function can change the future of the computation based
on the `density` parameter.

This is not possible with `Functor` and `Applicative`.

## TLDR

Function application transforms one value into another.

`Functor` allows those transformations to happen inside a context.

`Applicative` also allows such computations to *accumulate* that context.

`Monad` also allows such computations to change the shape of that context.
