# Output

Code: [Output.hs](../src/Output.hs)

Uncomment this line in [edu-wumpus.cabal](../edu-wumpus.cabal):

    -- , Output

Uncomment this line in [doctest/Main.hs](../doctest/Main.hs):

    -- test "src/Output.hs"

Rebuild [the documentation](../INSTALL.md):

    $ cd /path/to/edu-wumpus
    $ stack haddock

Point your browser at `/path/to/edu-wumpus/doc/index.html`.
Check out the documentation for `Output`.

    $ stack test

You will see that there are, indeed, unimplented functions in this file.
Implement them!

## What's going on

This file is all output functions. You will be using a lot of `putStrLn`.  You
may be able to figure out some shortcuts to avoid too much copy and paste.

Take note of the `printPrompt` function, which requires that you flush the
output buffer using `hFlush stdout`. I've put some hints in the file
documentation, and added the correct `import` statement.

For the most part, follow the test outputs and you will be fine.
You might want to review [the notes on
`RecordWildCards`](Cave.md#recordwildcards).

