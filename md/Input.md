# Input

Code: [Input.hs](../src/Input.hs)

Uncomment this line in [edu-wumpus.cabal](../edu-wumpus.cabal):

    -- , Input

Uncomment this line in [doctest/Main.hs](../doctest/Main.hs):

    -- test "src/Input.hs"

Rebuild [the documentation](../INSTALL.md):

    $ cd /path/to/edu-wumpus
    $ stack haddock

Point your browser at `/path/to/edu-wumpus/doc/index.html`.
Check out the documentation for `Input`.

    $ stack test

You will see that there are, indeed, unimplented functions in this file.
Implement them!

## What's going on

This file defines `getAction`, which just calls `hGetAction`, the function you
actually have to implement. By passing a `Handle` to this function and using
`hGetLine` instead of `getLine`, we can test this function on handles other
than `stdio`.

The implementation of this function requires the monadic context, and making
choices on whether to return a value or retry, based on whether `parseAction`
was able to successfully parse the input string.

Some hints you may find useful:

* You will probably want to use some auxiliary functions.

* Since `parseAction` returns a `Maybe Action`, you have a couple choices of
  how you handle that value. You could use an auxiliary function:

        foo Nothing  = ...
        foo (Just a) = ...

  You can use a `case` expression:

        case ma of
            Nothing  -> ...
            (Just a) -> ...

  You can use the `maybe` operator:

        maybe (...) (...) ma
