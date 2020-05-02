### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### Setup
May need to do some special stuff to get the trivial-gamekit package working, see: https://github.com/borodust/trivial-gamekit

### TODO
* (Maybe) Beam search state caching, avoids wasting time on already-explored paths.
* Evolve heuristic parameters for greedy search.
* Get best possible performance (increase heap size, run on computer, tweak beam width / search depth).
* Evolve heuristic parameters for beam search, optimize performance (like b4).
* If that didn't break any records... use existing records as a runway.
* (Maybe) Try Monte Carlo Tree Search.

### Credits
Jeffrey Massung for his queue implementation (https://github.com/massung/queue), which I copy/pasted here (with the addition of a new method).
