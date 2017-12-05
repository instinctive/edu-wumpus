# Installing

[Install Haskell Stack](https://docs.haskellstack.org/en/stable/README/), if you haven't already:

    $ curl -sSL https://get.haskellstack.org/ | sh
    $ stack setup

[Fork this repo on GitHub](https://help.github.com/articles/fork-a-repo/), so
that others can comment on your solutions, raise issues, etc.

Clone **your fork** of this repo and **build the documentation**:

    $ git clone https://github.com/YOUR-GITHUB-ID/edu-wumpus.git
    $ cd edu-wumpus
    $ stack haddock
    $ ln -s `stack path --local-doc-root`

Now you can point your browser at the Haddock documentation for this project on your local machine:

    /home/YOURNAME/path/to/edu-wumpus/doc/index.html

As you proceed in this project, you will want to re-build the documentation, as more will be added:

    $ stack haddock

[Let's get started!](md/Wumpus.md)
