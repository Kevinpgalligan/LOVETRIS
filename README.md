### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### Setup
trivial-gamekit package has manual installation steps, see: https://github.com/borodust/trivial-gamekit

### TODO
* Write own queue implementation.
* Make code thread-safe again.
* Search tree stats: node depths, distribution of heuristic scores, average ply, number of nodes / space occupied. This can be used to inform what sorta search depth will be feasible.
* Evolve parameters for brute force search, see how good it can get.
* Springboard, use existing records as a runway.
* MCTS (trade-off between reward & exploration).

### Usage
(Assuming you've done `(in-package lovetris)` in the REPL).

Play the game (using controls).

```lisp
(play-hatetris)
```

I'll add more examples here. In the meantime, there are slightly outdated examples in docs/profiling.txt.
