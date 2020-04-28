### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### Setup
May need to do some special stuff to get the trivial-gamekit package working, see: https://github.com/borodust/trivial-gamekit

### TODO
1. ~~Multi-threading (cl-threadpool is shit, e.g. no way to wait for jobs to finish. It just doesn't have a rich API. Better to use custom bordeaux-threads solution, I guess).~~ BUT: think through semantics before/after, understand why the score changed.
2. Implement evolutionary algorithm for optimising greedy search, update article with the best greedy parameters I can find (video, explanation).
3. (Possibility) Beam search state caching, would avoid wasteful computation.
4. Find best combination of beam width & search depth.
5. Evolve better parameters for beam search (most of the work should have been done in step 2).
6. Write up beam search results.
7. Use existing records as a runway.
8. Try Monte Carlo Tree Search.

### Credits
Jeffrey Massung for his queue implementation (https://github.com/massung/queue), which I copy/pasted here (with the addition of a new method).
