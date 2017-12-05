# Play

Code: [Play.hs](../src/Play.hs)

Uncomment this line in [edu-wumpus.cabal](../edu-wumpus.cabal):

    -- , Play

Uncomment this line in [doctest/Main.hs](../doctest/Main.hs):

    -- test "src/Play.hs"

Rebuild [the documentation](../INSTALL.md):

    $ cd /path/to/edu-wumpus
    $ stack haddock

Point your browser at `/path/to/edu-wumpus/doc/index.html`.
Check out the documentation for `Play`.

    $ stack test

You will see that there are, indeed, unimplented functions in this file.
Implement them!

## What's going on

You now have all the pieces you need to implement `play`. You will create the
`Game` from the `CaveMap` using `mkGame`. Recall that `mkGame` has a
`MonadRandom` constraint:

    mkGame :: MonadRandom m => CaveMap -> m Game

Now you are going to use this function in the `IO` context. As
[discussed](Cave.md#monadrandom), the `MonadRandom` instance for `IO` is
already defined, so everything "will just work."

Just as with `hGetAction` from [Input.sh](../src/Input.hs), you will have to
deal with peeling back the layers of a return value, in this case the `Status`
value from `doAction`.
