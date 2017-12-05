# Guide to the Wumpus-Hunt

In this exercise, you will build a reduced version of the [original
game](https://www.atariarchives.org/bcc1/showpage.php?page=247).

Here's an [example of play](Example.md).

The code for this problem is broken up into several modules. We will do the
exercise module-by-module, in this order:

* [Cave](Cave.md)
* [Common](Common.md)
* [Game](Game.md)
* [Output](Output.md)
* [Parse](Parse.md)
* [Input](Input.md)
* [Play](Play.md)
* [Main](Main.md)

After all modules have been completed, the game will be playable:

    $ stack build
    $ stack exec wumpus-exe
