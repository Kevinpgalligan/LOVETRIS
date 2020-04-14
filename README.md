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
8. Some way of replaying games, in order to analyze AI behaviour.
9. Analyze redundancy in beam search, i.e. how often do the branches converge. What percentage of states are duplicates in the fully-expanded tree. Depending on the results of this analysis, it might be worth implementing caching to avoid wasting time on duplicate paths / to encourage diversity.
10. Investigate performance limits: time to advance a step for increasing search depth and beam width. And the scores achieved by each of them. If deemed worthwhile, profile code and improve its performance.
11. Rework genetic algo code to find best heuristic weights for beam search.
12. Monte Carlo Tree Search.
