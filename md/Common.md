# Common

Resources:

* [Common.hs](../src/Common.hs)

Uncomment this line in [edu-wumpus.cabal](../edu-wumpus.cabal):

    -- , Common

Uncomment this line in [doctest/Main.hs](../doctest/Main.hs):

    -- test "src/Common.hs"

Rebuild [../INSTALL.md](the documentation):

    $ cd /path/to/edu-wumpus
    $ stack haddock

Point your browser at `/path/to/edu-wumpus/doc/index.html`.

## What's going on

There's nothing unimplemented in this file. It's a set of types
that define the interaction between the game and the player.
