### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### Setup
May need to do some special stuff to get the trivial-gamekit package working, see: https://github.com/borodust/trivial-gamekit

### TODO
* Add a way to save / reload population, to enable the workflow: run for a few rounds, check how it's doing, run for a few rounds, etc.
* Logging to see progress.
* Implement node cache, should speed things up.
* Kick off evolution of better parameters.
  (To consider: evolve them using beam search, or greedy search? Need to think it through).
* FINAL RUN!!!!!! Exciting. Carefully consider beam width, search depth. And increase heap size.
* If that didn't break any records, use existing records as a runway.
* (Maybe) Try MCTS.

### Credits
Jeffrey Massung for his queue implementation (https://github.com/massung/queue), which I copy/pasted here (with the addition of a new method).
