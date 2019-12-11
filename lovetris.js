var MOVE_DELAY = 0.2;

function runLovetris() {
    var cells = [].slice.call(document.getElementsByClassName("hatetris__cell"));
    var buttons = {
        "left": getButton(cells, "Press Left"),
        "right": getButton(cells, "Press Right"),
        "down": getButton(cells, "Press Down"),
        "rotate": getButton(cells, "Press Up")
    }
    var gameState = initGameState();
    while (gameState.running) {
        gameState = updateGameState(gameState);
        var moves = gameState.lastMoveSequence();
        moves.forEach(move => {
            buttons[move].click();
            // TODO fix this, only works in async function and
            // sleep() is undefined
            await sleep(MOVE_DELAY);
        });
    }
}

function getButton(cells, titleSubstring) {
    return cells.find(element => element.title.includes(titleSubstring));
}

function initGameState() {
    // TODO
    // initialize model of board and search tree
}

function updateGameState(gameState) {
    // TODO
    // find best move & generate sequence of steps to get
    // there
}
