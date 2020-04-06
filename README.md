### Description
An AI to play this Tetris variant: https://qntm.org/files/hatetris/hatetris.html

### TODO
1. ~~Core data structures (board, pieces, piece orientations, piece moves)~~
2. ~~Algorithm to find valid positions of a piece.~~
3. ~~Hatetris AI: look at all possible moves for all possible pieces, pick the one the maximises the minimum possible height. Tie breaks resolved by order of pieces. I think. Need to review the HATETRIS code.~~
4. GUI that lets player pick moves (need to add "on-start" and "on-end" functions to the game interface that allow GUI to be booted up / shut down; implement a version of "place-piece" that gets the player to manipulate the piece through the GUI; possibly adjust what the game interface returns at the end, as right now it just returns the final state, which does not allow replays).
5. Play through some games myself to test it (incl. world record games). Hopefully this won't turn up any bugs.
6. Greedy search (algorithm and heuristics).
7. Rework genetic algo code to find best heuristic weights.
8. Implement Monte Carlo Tree Search.
