namespace FSAI

module Minimax =
    let minValue = -2147483648
    let maxValue = 2147483647
    let Empty: byte = 0uy
    let White: byte = 1uy
    let Black: byte = 2uy
    let Tie: byte = 4uy

    // Gets the score of the current player
    let rec GetScore (board:  byte[,]) tile x y score = 
        //Basecases for recersive function
        if x < 8 then
            if y < 8 then
                if board.[x, y] = tile then
                    GetScore board tile x (y+1) (score+1)
                else 
                    //steps to next square
                    GetScore board tile x (y+1) score
            else 
                GetScore board tile (x+1) 0 score
        else
            score

    // Checks if x y is within the board limits
    let IsOnBoard x y =
        0 <= x && x <= 7 && 0 <= y && y <= 7

    // Swaps tile
    let OtherTile tile = 
        if tile = Black then
            White
        else 
            Black
        
    // Searches for your tile on the other side of the opponents tile
    let rec whileTileIsOpponent (board:  byte[,]) tile x y X Y i j = 
        if IsOnBoard x y then
            if board.[x, y] = tile then
                [(X, Y)]
            else if board.[x, y] = Empty then
                []
            else
                //recersive function what walks in a given direction until it finds an empty square
                whileTileIsOpponent board tile (x-1+i) (y-1+j) X Y i j
        else
            []
            
    // Searches for empty square with opponent next to it
    let rec forEachValidMove (board:  byte[,]) tile X Y i j validMove = 
        if List.isEmpty validMove then
        //Basecases for recersive function
            if i < 3 then
                if j < 3 then
                    //skips the current square in the middle of the recursive loop
                    if i = 1 && j = 1 then
                        forEachValidMove board tile X Y i (j+1) validMove 
                    else
                         let x = X - 1 + i
                         let y = Y - 1 + j
                         if IsOnBoard x y && board.[x, y] = OtherTile tile then
                            let x1 = x - 1 + i
                            let y1 = y - 1 + j
                            let emptyOrValid = whileTileIsOpponent board tile x1 y1 X Y i j
                            forEachValidMove board tile X Y i (j+1) (emptyOrValid)
                         else
                            forEachValidMove board tile X Y i (j+1) validMove
                else
                    forEachValidMove board tile X Y (i+1) 0 validMove
            else
                []
        else
            validMove
    
    // Gets valid moves of current board
    let rec GetValidMoves (board:  byte[,]) tile X Y (validMoves: (int * int)list) = 
        //Basecases for recersive function
        if X < 8 then
            if Y < 8 then
                if board.[X, Y] = Empty then
                    //adds a valid move to the list valid moves
                    let thisValidMove:(int * int)list = forEachValidMove board tile X Y 0 0 []
                    GetValidMoves board tile X (Y+1) (validMoves @ thisValidMove) 
                else
                    //steps to next square
                    GetValidMoves board tile X (Y+1) validMoves
            else
                GetValidMoves board tile (X+1) 0 validMoves
        else
            validMoves

    // Returns a winner or tie
    let GetWinner board = 
        let blackScore = GetScore board Black 0 0 0
        let whiteScore = GetScore board White 0 0 0
        if blackScore = 0 || whiteScore = 0 || blackScore+whiteScore = 64 || (List.length (GetValidMoves board Black 0 0 []) + List.length (GetValidMoves board White 0 0 [])) = 0 then
            if blackScore > whiteScore then
                Black
            else if whiteScore > whiteScore then
                White
            else
                Tie
        else Empty


    // Checks if corners are occupied by a tile
    let cornerPositions = [(0, 0); (0, 7); (7, 0); (7, 7)]
    let rec CountCorners (board:  byte[,]) tile i corners = 
        if i < 4 then
            let cornerPos = List.tryItem i cornerPositions
            let (x, y) = cornerPos.Value
            if board.[x, y] = tile then
                CountCorners board tile (i+1) (corners+1)
            else
                CountCorners board tile (i+1) corners
        else
            corners

    // Evaluates board
    let Evaluation board = 
        let blackScore = GetScore board Black 0 0 0
        let whiteScore = GetScore board White 0 0 0
        let blackMobility = List.length(GetValidMoves board Black 0 0 [])
        let whiteMobility = List.length(GetValidMoves board White 0 0 [])

        if blackScore = 0 then
            -200000
        else if whiteScore = 0 then
            200000
        else
            if blackScore + whiteScore = 64 || blackMobility + whiteMobility = 0 then
                if 2 < whiteScore then
                    -100000 - whiteScore + blackScore
                else if blackScore > whiteScore then
                    100000 + blackScore - whiteScore
                else
                    0
            else
                let evaluation = blackScore - whiteScore

                if blackScore + whiteScore > 55 then
                    blackScore - whiteScore
                else
                    let evaluation2 = evaluation + (blackMobility - whiteMobility) * 10
                    let evaluation3 = evaluation2 + ((CountCorners board Black 0 0) - (CountCorners board White 0 0)) * 100
                    evaluation3

    // Steps over consecutive opponents tiles until it finds your tile 
    // and returns a list on flippable tiles or a empty list
    // if it finds a empty square
    let rec whileIsOnBoard (board:  byte[,]) tile x y i j dirFlippedPieces = 
        if IsOnBoard x y then
            //returns a list of consecutive opponents tiles
            if board.[x, y] = tile then
                dirFlippedPieces
            else
                //creates a list of consecutive opponents tiles
                if board.[x, y] = Empty then
                    []
                else
                    //steps to next square
                    whileIsOnBoard board tile (x-1+i) (y-1+j) i j (dirFlippedPieces@[(x, y)])
        else
            []

    // Searches for squares with opponent tile next to a empty square 
    // and calls whileIsOnBoard to get a list of opponent tiles to flip in all valid directions
    let rec forEachDir (board:  byte[,]) tile moveX moveY i j flippedPieces =
        //Basecase to stop recursion
        if i < 3 then
            if j < 3 then
                let x = moveX - 1 + i
                let y = moveY - 1 + j
                if IsOnBoard x y && board.[x, y] = OtherTile tile then
                    let dirFlippedPieces = [(x, y)]
                    let x1 = x - 1 + i
                    let y1 = y - 1 + j
                    //Creates a list of flippebale tiles
                    let newDirFlippedPieces = whileIsOnBoard board tile x1 y1 i j dirFlippedPieces
                    forEachDir board tile moveX moveY i (j+1) (newDirFlippedPieces@flippedPieces)
                else
                //steps to next square
                    forEachDir board tile moveX moveY i (j+1) flippedPieces
            else 
                forEachDir board tile moveX moveY (i+1) 0 flippedPieces 
        else   
            //returns the list
            flippedPieces

    // Gets flipped pieces on current board
    let GetFlippedPieces (board:  byte[,]) (move: int*int) (tile: byte) =
        let (moveX, moveY) = move
        if board.[moveX, moveY] = Empty then
            forEachDir board tile moveX moveY 0 0 []
        else 
            []

    // Swaps the color of a tile after it was flipped 
    let rec forEachFlippedPiece (board:  byte[,]) tile i flippedPieces = 
        if i < List.length flippedPieces then
            let flippedPiece = List.tryItem i flippedPieces
            let (x, y) = flippedPiece.Value
            board.[x, y] <- tile
            forEachFlippedPiece board tile (i+1) flippedPieces

    // places the players move tile after it flips all flipped pices
    let MakeMove (board:  byte[,]) (move: int*int) (tile: byte) =
        let flippedPieces = GetFlippedPieces board move tile
        forEachFlippedPiece board tile 0 flippedPieces
        if not(List.isEmpty flippedPieces) then
            let (moveX, moveY) = move
            board.[moveX, moveY] <- tile

    // Uses all other functions and a recursive MiniMax function with Alpha Beta pruning
    // to find the best move the ai can place with the given depth
    let rec MiniMaxAlphaBeta (board: byte[,]) depth a b tile isMaxPlayer = 
        if depth = 0 || GetWinner(board) <> Empty then
            Evaluation(board)
        else
            let bestScore =   
                if isMaxPlayer then
                    minValue
                else
                    maxValue
            let validMoves = GetValidMoves board tile 0 0 []
            if not(List.isEmpty validMoves) then
                // Creates a new for recursive function inside MiniMaxAlphaBeta
                // to help us call them both without one being out of scope when declared after the other
                let rec forEachMove a b bestScore isMaxPlayer tile i =
                    if i < List.length validMoves then
                        let move = List.tryItem i validMoves
                        let childBoard = Array2D.copy board
                        MakeMove childBoard move.Value tile
                        let nodeScore = MiniMaxAlphaBeta childBoard (depth - 1) a b (OtherTile tile) (not isMaxPlayer)
                        if isMaxPlayer then
                            let newBestScore = max bestScore nodeScore
                            let newA = max newBestScore a
                            if b <= newA then
                                newBestScore
                            else
                                forEachMove newA b newBestScore isMaxPlayer tile (i+1)
                        else
                            let newBestScore = min bestScore nodeScore
                            let newB = min newBestScore b
                            if newB <= a then
                                newBestScore
                            else
                                forEachMove a newB newBestScore isMaxPlayer tile (i+1)               
                    else 
                        bestScore
                forEachMove a b bestScore isMaxPlayer tile 0
            else
                MiniMaxAlphaBeta board depth a b (OtherTile tile) (not isMaxPlayer)


    type Class1() = 
        member this.X = "F#"