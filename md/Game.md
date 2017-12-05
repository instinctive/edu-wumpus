# Game

Resources:

* [Game.hs](../src/Game.hs)

Uncomment this line in [edu-wumpus.cabal](../edu-wumpus.cabal):

    -- , Game

Uncomment this line in [doctest/Main.hs](../doctest/Main.hs):

    -- test "src/Game.hs"

Rebuild [the documentation](../INSTALL.md):

    $ cd /path/to/edu-wumpus
    $ stack haddock

Point your browser at `/path/to/edu-wumpus/doc/index.html`.
Check out the documentation for `Game`.

    $ stack test

You will see that there are, indeed, unimplented functions in this file.
Implement them!

## What's going on

The previous file, [Common.hs](../src/Common.hs) defined the `View` type. This
file defines the `Game` type, which is the actual game state.

The central logic about how player actions change the game state is in
the `doAction` function.

Our old friend `MonadRandom` is back for the `mkGame` function, which needs to
use the `rndCave` function we defined in [Cave.hs](../src/Cave.hs).  Notice
that these monadic contexts are contagious. Because `rndCave` has a
`MonadRandom` constraint, so does `mkGame`.

You may want to review [the notes about `MonadRandom`](Cave.md#monadrandom),
as well as the [Functor, Applicative, and Monad](Monad.md) refresher.
