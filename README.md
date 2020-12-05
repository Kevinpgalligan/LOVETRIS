### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### Setup
May need to do some special stuff to get the trivial-gamekit package working, see: https://github.com/borodust/trivial-gamekit

### TODO
* Possibly: nodes can have multiple parents, backlink to them. This will be required for MCTS, anyway.
* Evolve parameters for brute force search, see how good can get.
* Nice utility: visualise AI playing.
* Possible utility: search tree stats (state depths; distribution of heuristic scores; ...).
* Springboard, use existing records as a runway.
* MCTS (trade-off between reward & exploration).

### Usage
(Assuming you've done `(in-package lovetris)` in the REPL).

Play the game (using controls).

```lisp
(play-game)
```

There are also a bunch of (possibly outdated) examples in profiling/profiling.txt.