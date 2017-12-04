# Installing

[Install Haskell Stack](https://docs.haskellstack.org/en/stable/README/), if you haven't already:

    $ curl -sSL https://get.haskellstack.org/ | sh
    $ stack setup

Now clone this repo and build the documentation:

    $ git clone https://github.com/instinctive/edu-wumpus.git
    $ cd edu-wumpus
    $ stack haddock
    $ ln -s `stack path --local-doc-root`

Now you can point your browser at the Haddock documentation for this project on your local machine:

    /home/YOURNAME/path/to/edu-wumpus/doc/index.html

As you proceed in this project, you will want to re-build the documentation, as more will be added:

    $ stack haddock
