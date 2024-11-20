import Data.List
import Data.Ord (comparing)

type Point = (Int, Int)
type Line = (Point, Direction)
data Direction = Vertical | Horizontal deriving (Eq, Show)
type Box = (Point, Player)
type Board = [Line]

-- Game type is a 4-tuple containing the complete board, the active player, the completed boxes, and the history of moves
type Game = (Board, Player, [Box], [Move])
data Player = PlayerOne | PlayerTwo deriving (Eq, Show)
type Move = Line
data Winner = Winner Player | Draw deriving (Show)

--calcBoard to create a board from a given int size (always a perfect square)
calcBoard :: Int -> Board
calcBoard size = ([((x,y), Horizontal) | x <- [0..size - 1], y <- [0..size]] ++ [((x,y), Vertical) | x <- [0..size], y <- [0..size - 1]])

--Helper functions to get values from Game type
gameBoard :: Game -> Board
gameBoard (board, _, _, _) = board
gamePlayer :: Game -> Player
gamePlayer (_, player,_, _) = player
gameBoxes :: Game -> [Box]
gameBoxes (_, _, boxes, _) = boxes
gameMoves :: Game -> [Move]
gameMoves (_, _, _, moves) = moves

--hard coding values: ((x, y), Direction) for 4x4 grid
-- (0-3),(0-4) Horizontal
-- (0-4),(0-3) Vertical
allLines :: [Line]
allLines = [((0,0), Horizontal), ((0,1), Horizontal), ((0,2), Horizontal), ((0,3), Horizontal), ((0,4), Horizontal), ((1,0), Horizontal), ((1,1), Horizontal), ((1,2), Horizontal), ((1,3), Horizontal), ((1,4), Horizontal), ((2,0), Horizontal), ((2,1), Horizontal), ((2,2), Horizontal), ((2,3), Horizontal), ((2,4), Horizontal), ((3,0), Horizontal), ((3,1), Horizontal), ((3,2), Horizontal), ((3,3), Horizontal), ((3,4), Horizontal), ((0,0), Vertical), ((1,0), Vertical), ((2,0), Vertical), ((3,0), Vertical), ((4,0), Vertical), ((0,1), Vertical), ((0,2), Vertical), ((0,3), Vertical), ((1,1), Vertical), ((1,2), Vertical), ((1,3), Vertical), ((1,4), Vertical), ((2,1), Vertical), ((2,2), Vertical), ((2,3), Vertical), ((2,4), Vertical), ((3,1), Vertical), ((3,2), Vertical), ((3,3), Vertical), ((3,4), Vertical)]

-- temporary declaration to test functions - This is a game state example
gameStateBoxes = [((0,0), PlayerTwo), ((0,1), PlayerTwo), ((1,1), PlayerOne)]
gameStateMoves = [((0,0), Horizontal), ((2,0), Horizontal), ((0,1), Horizontal), ((0,2), Horizontal), ((1,1), Horizontal), ((1,2), Horizontal), ((2,3), Horizontal), ((0,0), Vertical), ((0,1), Vertical), ((1,0), Vertical), ((1,1), Vertical), ((1,2), Vertical), ((2,1), Vertical), ((3,1), Vertical)]
gameStateExample :: Game
gameStateExample = (allLines, PlayerOne, gameStateBoxes, gameStateMoves)

-- to calc all lines in given box. input box of ((0,0), Player1)
-- TO DO : When checking if a box is Valid you only need to check first line and the vertical of that one Ex. check (0,0, Horizontal), (0,0, Vertical)
calcBox :: Box -> [Line]
calcBox box =
    let point = fst box
        horPoint = (fst point, snd point + 1)
        vertPoint = (fst point + 1, snd point)
    in [boxLines | boxLines <- allLines, (point, Horizontal) == boxLines || (point, Vertical) == boxLines || (horPoint, Horizontal) == boxLines || (vertPoint, Vertical) == boxLines]

-- Story 2 : Determine who has won the game. Write a function Game -> Winner
-- Ana, Adrian

gameWinner :: Game -> Maybe Winner
gameWinner game =
    let boxes = gameBoxes game
        gameOver = null (legalMoves game)
        p1Points = length [box | box <- boxes, snd box == PlayerOne]
        p2Points = length [box | box <- boxes, snd box == PlayerTwo]
    in if p1Points > p2Points && gameOver then Just (Winner PlayerOne) else if p2Points > p1Points && gameOver then Just (Winner PlayerTwo) else if p1Points == p2Points && gameOver then Just Draw else Nothing

-- Story 3 : Compute the result of making a legal move in a game state, write a function of type
-- Emma 
makeMove :: Game -> Move -> Game
makeMove game move =
    let board = gameBoard game
        player = gamePlayer game
        boxes = gameBoxes game
        moves = gameMoves game

        -- updates moves by adding the new move
        newMoves = move : moves

        -- checks if a specific box is completed
        isBoxCompleted :: Point -> Bool
        isBoxCompleted point =
            let top = (point, Horizontal)
                bottom = ((fst point , snd point +1), Horizontal)
                left = (point, Vertical)
                right = ((fst point + 1 , snd point), Vertical)
            in  all (`elem` newMoves) [top, bottom, left, right]

        -- collects the completed boxes after the move
        completedBoxes = [(point, player) | point <- [(x, y) | x <- [0..3], y <- [0.. 3]],
                           isBoxCompleted point,
                           point `notElem` map fst boxes]

        -- updates the boxes
        newBoxes = completedBoxes ++ boxes

        -- Determines next player
        nextPlayer = if null completedBoxes then switchPlayer player
                     else player

    in (board, nextPlayer, newBoxes, newMoves)

-- Helper to switch player
switchPlayer :: Player -> Player
switchPlayer PlayerOne = PlayerTwo
switchPlayer PlayerTwo = PlayerOne



-- Story 4 : Compute the legal moves from a game state, use a function Game -> [Move]
-- Gael, Molly
legalMoves :: Game -> [Move]
legalMoves game =
    let board = gameBoard game
        moveHistory = gameMoves game
        legalLines = [lines | lines <- board, lines `notElem` moveHistory]
    in legalLines

isLegalMove :: Game -> Move -> Bool
isLegalMove game move =
    let moveHistory = gameMoves game
        board = gameBoard game
    in move `notElem` moveHistory && move `elem` board

-- Story 5 : Pretty-print a game into a string, create a function Game -> String
-- you should NOT override the "Show" typeclass. 
-- Aidan
prettyPrint :: Game -> String
prettyPrint (board, _, boxes, moves) = unlines $ concatMap renderRow [0 .. size]
  where
    -- determine board size dynamically
    size = 2


    -- Render a single row of the game
    renderRow :: Int -> [String]
    renderRow row
      | row < size = [renderHorizontal row, renderVertical row]
      | otherwise = [renderHorizontal row] -- only horizontal line for the last row

    -- render horizontal lines and dots for a specific row
    renderHorizontal :: Int -> String
    renderHorizontal row =
      concatMap (\col -> renderDot ++ renderHLine (col, row)) [0 .. size -1 ] ++ renderDot

    -- render vertical lines and boxes for a specific row
    renderVertical :: Int -> String
    renderVertical row =
      concatMap (\col -> renderVLine (col, row) ++ renderBox (col, row)) [0 .. size -1 ]
      ++ renderVLine (row, size-1) -- Last vertical line in the row

    -- render a dot
    renderDot :: String
    renderDot = "."

    -- render either a horizontal line or space
    renderHLine :: Point -> String
    renderHLine p =
      if (p, Horizontal) `elem` moves
        then "---"
        else "   "


    -- render either a vertical line or space
    renderVLine :: Point -> String
    renderVLine p =
      if (p, Vertical) `elem` moves
        then "|"
        else " "


    -- render box with ownership or empty space
    renderBox :: Point -> String
    renderBox p =
      case lookup p boxes of
        Just PlayerOne -> " P1"
        Just PlayerTwo -> " P2"
        Nothing -> "   "


-- Story 6 : All functions should consider possible errors or edge cases

-- Story 9 : whoWillWin :: Game -> Winner
--close game considers a very close game on a 2x2 board
closeGame :: Game
closeGame = (calcBoard 2, PlayerOne, [((1,1), PlayerTwo)], [((0,0), Vertical), ((2,0), Vertical), ((0,1), Vertical), ((1,1), Vertical), ((2,1), Vertical), ((0,0), Horizontal), ((0,1), Horizontal), ((1,1), Horizontal), ((1,2), Horizontal)])

--whoWillWin function is a function that simulates both players playing optimally(here we cannot),
--score system accounting for boxes
whoWillWin :: Game -> Winner
whoWillWin game =
  let validMoves = legalMoves game
      moveEvaluations = [moveEvaluation game move | move <- validMoves]
      optimalResult = snd (maximumBy (comparing fst) moveEvaluations)
  in case gameWinner optimalResult of
    Just (Winner PlayerOne) -> Winner PlayerOne
    Just (Winner PlayerTwo) -> Winner PlayerTwo
    Just Draw -> Draw
    Nothing -> whoWillWin optimalResult

-- this only goes on in the case of the first one, think of something that goes through everything
-- also, there are definitely better moves than others, find a move that gives you the greatest number of boxes.
moveEvaluation :: Game -> Move -> (Int, Game)
moveEvaluation game@(board,currentPlayer, boxes, moveHistory) move =
  let newGame@(newBoard, newPlayer, newBoxes, newMoveHistory) = makeMove game move
      score = length [box | box <- newBoxes, snd box == currentPlayer]
  in  (if newPlayer == currentPlayer && not (null (legalMoves newGame)) then uncurry moveEvaluation (contGame newGame) else (score, newGame))
  --recurse this in case of move keeping player there
contGame :: Game -> (Game, Move)
contGame game = (game, head (legalMoves game))

--moveOutcome :: Game -> Move -> Winner
--moveOutcome game move =
--  let newGame = makeMove game move
--      newMove:valids = legalMoves newGame
--  in case gameWinner newGame of
--    Just (Winner PlayerOne) -> Winner PlayerOne
--    Just (Winner PlayerTwo) -> Winner PlayerTwo
--    Just Draw -> Draw
--    Nothing -> moveOutcome newGame newMove

-- find the result of all possible moves, then compare those to get the one that benefits the current player the most by the end
--of that "turn". get the optimal move then do that again