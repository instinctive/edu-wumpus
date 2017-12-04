# Example

Here's an example of the Wumpus game in play:

    $ stack exec wumpus-exe
    You are in cave 0, tunnels lead to 3 1 4
    You smell a wumpus!
    What do you do? move 3
    You are in cave 3, tunnels lead to 2 0 7
    What do you do? move 2
    You are in cave 2, tunnels lead to 1 3 6
    What do you do? move 1
    You are in cave 1, tunnels lead to 0 2 5
    What do you do? move 0
    You are in cave 0, tunnels lead to 3 1 4
    You smell a wumpus!
    What do you do? shoot 4
    You killed the Wumpus! The villagers laud you as a hero!

How the game went:

* I started in 0 with the Wumpus nearby, so my first move to 3 was a risk.
* I survived, which meant the Wumpus must be in 1 or 4.
* When I moved to cave 2 I could see a tunnel to cave 1, but I couldn't smell a
Wumpus. So 1 must also be safe.
* I now knew the Wumpus must have been in 4. I backtracked and shot it.
