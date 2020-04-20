### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### Setup
May need to do some special stuff to get the trivial-gamekit package working, see: https://github.com/borodust/trivial-gamekit

### TODO
1. ~~Core data structures (board, pieces, piece orientations, piece moves)~~
2. ~~Algorithm to find valid positions of a piece.~~
3. ~~Hatetris AI: look at all possible moves for all possible pieces, pick the one the maximises the minimum possible height. Tie breaks resolved by order of pieces. I think. Need to review the HATETRIS code.~~
4. ~~GUI that lets player pick moves (need to add "on-start" and "on-end" functions to the game interface that allow GUI to be booted up / shut down; implement a version of "place-piece" that gets the player to manipulate the piece through the GUI; possibly adjust what the game interface returns at the end, as right now it just returns the final state, which does not allow replays).~~
5. ~~Play through some games myself to test it (incl. world record games). Hopefully this won't turn up any bugs.~~
6. ~~Greedy search (algorithm and heuristics).~~
7. ~~Beam search.~~
8. ~~Some way of replaying games, in order to analyze AI behaviour.~~
9. ~~Analyze redundancy in beam search, i.e. how often do the branches converge. What percentage of states are duplicates in the fully-expanded tree. Depending on the results of this analysis, it might be worth implementing caching to avoid wasting time on duplicate paths / to encourage diversity.~~
10. ~~Fill in the article in chronological order: write what I can / make gameplay videos; implement random search, show video; implement blind greedy search, show video.~~
11. Make sure that it can run a full game to completion; if not, try again with increased heap size.
12. Low-hanging fruit optimisation, if such fruit exists.
13. Implement evolutionary stuff (generic version) for optimising greedy search, update article with the best greedy parameters I can find (video, explanation).
14. Beam search state caching.
15. Optimise / profiling / multi-threading, find best combination of beam width & search depth.
16. Evolve better parameters for beam search.
17. Write up final beam search results. If record-breaking, leave it at that. If not, move on to Monte Carlo.

### Credits
Jeffrey Massung for his queue implementation (https://github.com/massung/queue), which I copy/pasted here (with the addition of a new method).
