# Parse

Code: [Parse.hs](../src/Parse.hs)

Uncomment this line in [edu-wumpus.cabal](../edu-wumpus.cabal):

    -- , Parse

Uncomment this line in [doctest/Main.hs](../doctest/Main.hs):

    -- test "src/Parse.hs"

Rebuild [the documentation](../INSTALL.md):

    $ cd /path/to/edu-wumpus
    $ stack haddock

Point your browser at `/path/to/edu-wumpus/doc/index.html`.
Check out the documentation for `Parse`.

    $ stack test

You will see that there are, indeed, unimplented functions in this file.
Implement them!

## What's going on

There is a single function in this file, `parseAction`. It's objective is to
return an `Action` given a string. The test examples should show what's going
on.

If the string is, for example, `"move 4"`, the `"4"` can be converted to the
integer `4` using `readMaybe`, and then further converted to a `Cave` using the
`Cave` constructor.
