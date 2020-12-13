### Description
An AI that plays HATETRIS: <https://qntm.org/hatetris>

### Setup
trivial-gamekit package has manual installation steps, see: https://github.com/borodust/trivial-gamekit

### TODO
* Search tree stats: node depths, distribution of heuristic scores, average ply, number of nodes / space occupied. This can be used to inform what sorta search depth will be feasible.
* Another round of evolution.
* Run the game w/ search depth as big as possible.
* Springboard, use existing records as a runway.
* MCTS (trade-off between reward & exploration).

### Usage
(Assuming you've done `(in-package lovetris)` in the REPL).

Play the game (using controls).

```lisp
(play-hatetris)
```

I'll add more examples here. In the meantime, there are slightly outdated examples in docs/profiling.txt.
