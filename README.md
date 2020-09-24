A Breakout prototype, for exploring haskell game dev.
One of my first Haskell projects, from 2007, updated in 2020.

![screenshot](screenshot.png)

It should run on mac, windows, linux, and any platform with:

- GHC 8.10+
- SDL 2 C libs

It must be run from the source directory for now, to load files from `./data/*`.
An easy way to fetch dependencies, build and run:

    $ gh repo clone simonmichael/sm-breakout
    $ cd sm-breakout
    $ stack run
