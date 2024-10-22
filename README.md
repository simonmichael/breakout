A Breakout prototype, for exploring haskell game dev.
This was one of my first Haskell projects, in 2007, now updated in 2020.

![screenshot1](data/screenshot1.png)
![screenshot2](data/screenshot2.png)

## Features

- Runs on Windows, Mac, Unix (requires SDL 2 and GHC 8.10+)
- Standalone Executable (all assets included)
- 500 lines of Simple Haskell
- A Bat
- A Bouncing Ball
- Sound Effects
- High Score
- Pause

## Install

1. Install all of the main SDL 2 libs for your system ([1], [2]).
   Eg on mac: `brew install sdl2 sdl2_gfx sdl2_image sdl2_mixer sdl2_ttf` (but I'm having trouble with this on m1..)
2. `gh repo clone simonmichael/breakout`
3. `cd breakout`
4. `stack run`

[1]: https://www.libsdl.org/download-2.0.php
[2]: https://repology.org/project/sdl/badges


## Goals

While it continues... this project's goals are:

- Provide amusement and re-creation for myself
- Research, learn, uplevel my [haskell] game dev powers
- Experiment with tools, techniques, architectures, practices
- Minimise toil, maximise fun

<!--
and secondary/stretch goals would be:

- Provide new, maintained examples of a small haskell game implemented several ways, useful for learning or as starter templates, perhaps some docs
- Produce a more fun and impressive game that raises the bar for haskell games
- Stimulate more haskell game development
-->

## Help

I'm glad to receive your feedback or help via the #haskell-game chat channel.
Access it with an [IRC](https://webchat.freenode.net/#haskell-game) or [Matrix](https://matrix.to/#/#freenode_#haskell-game:matrix.org) client.

There is no issue tracker and no email support.
Pull requests may or may not get merged, at my discretion. 
Forks, complementary experiments, and non-costly collaboration are welcome.
(*Minimise toil*)

There's a [CHANGELOG.md](CHANGELOG.md) and a rough [TODO.md](TODO.md).
