# Testing

When you look at [Cave.hs](../src/Cave.hs), you'll see that some things are not
implemented. If you search in the file for `undefined`, you will see everything
that's not implemented:

    $ grep undefined src/Cave.hs
        show = undefined
    cave0 = undefined
    cave1 = undefined
    cave2 = undefined
    tunnels m = undefined
    rndCave CaveMap {..} = undefined
    triCave = undefined
    cubeCave = undefined

[This is your quest](https://www.youtube.com/watch?v=VZ42IMu7HIQ), to define
everything that is currently `undefined`.

How will you know if you have defined things correctly? Fortunately, I have created tests for the undefined functions. Here's how to run them:

    $ cd /path/to/edu-wumpus
    $ stack test

Try that right now. You will see a screen full of error messages like this:

    Testing "src/Cave.hs"
    ### Failure in src/Cave.hs:28: expression `show (Cave 2)'
    expected: "2"
     but got: "*** Exception: Prelude.undefined
              CallStack (from HasCallStack):
                error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
                undefined, called at src/Cave.hs:31:12 in main:Cave

The test is complaining that it was expecting a certain output, but it got a
`Prelude.undefined` exception instead. Once you correctly define these
functions, the errors will go away.
