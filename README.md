### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### Setup
May need to do some special stuff to get the trivial-gamekit package working, see: https://github.com/borodust/trivial-gamekit

### TODO
1. ~~Multi-threading to speed up search.~~
2. (Easy) Improve node API, differentiate between having no children and children not having been added yet.
3. (Maybe)(Medium) Beam search state caching, would avoid wasteful computation. Might be tricky to share the cache between threads, even though SBCL seems to come with a concurrent hash table. Can you attempt to set and see if it was already set, at the same time?
4. (Hard) Implement evolutionary algorithm for optimising greedy search, update article with the best greedy parameters I can find (video, explanation).
5. (Easy) Find best combination of beam width & search depth.
6. (Easy) Evolve better parameters for beam search (most of the work should have been done in step 2).
7. (Medium) Write up beam search results.
8. (Medium) Use existing records as a runway.
9. (Maybe)(Hard) Try Monte Carlo Tree Search.

### Credits
Jeffrey Massung for his queue implementation (https://github.com/massung/queue), which I copy/pasted here (with the addition of a new method).
