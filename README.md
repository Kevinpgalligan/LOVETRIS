### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### Setup
May need to do some special stuff to get the trivial-gamekit package working, see: https://github.com/borodust/trivial-gamekit

### TODO
* Figure out why replay isn't working.
* Make code thread-safe again.
* Nice-to-haves: visualise AI playing, as it is playing; search tree stats (state depths; distribution of heuristic scores; ...).
* Evolve parameters for brute force search, see how good it can get.
* Springboard, use existing records as a runway.
* MCTS (trade-off between reward & exploration).

### Usage
(Assuming you've done `(in-package lovetris)` in the REPL).

Play the game (using controls).

```lisp
(play-game)
```

There are also a bunch of (possibly outdated) examples in profiling/profiling.txt.