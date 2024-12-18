module SpookySquare where 

import Data.List
import Data.Ord

import Data.List.Split

type Point = (Int, Int)
type Line = (Point, Direction)
data Direction = Vertical | Horizontal deriving (Eq, Show)
type Box = (Point, Player)
type Board = [Line]
type Rating = Int

-- Game type is a 4-tuple containing the complete board, the active player, the completed boxes, and the history of moves
type Game = (Board, Player, [Box], [Move])
data Player = PlayerOne | PlayerTwo deriving (Eq, Show)
type Move = Line
data Winner = Winner Player | Draw deriving (Eq, Show)

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
        gameOver = null (legalMoves game) -- returns bool
        p1Points = length [box | box <- boxes, snd box == PlayerOne] --returns number
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
    size = 3


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
  
-- helper function to continue a game
contGame :: Game -> (Game, Move)
contGame game = (game, head (legalMoves game))




-- Story 10: BestMove 

-- helper to check if a move results in a win
isWinningMove :: Game -> Move -> Bool
isWinningMove game move =
  let newGame = makeMove game move
  in case gameWinner newGame of
       Just (Winner player) -> player == gamePlayer game
       Nothing -> False

-- helper function to check if a move leads to a tie
isForcingTie :: Game -> Move -> Bool
isForcingTie game move =
  let newGame = makeMove game move
      remainingMoves = legalMoves newGame
  in null remainingMoves && gameWinner newGame == Just Draw -- No moves left and no winner

--  bestMove 
bestMove :: Game -> Move
bestMove game =
  let moves = legalMoves game
      winningMoves = [move | move <- moves, isWinningMove game move]
      tieMoves = [move | move <- moves, isForcingTie game move]
  in case winningMoves of
       (wm:_) -> wm  -- return the first winning move
       [] -> case tieMoves of
               (tm:_) -> tm  -- Return the first move that forces a tie
               [] -> head moves -- Fallback to any legal move (shouldn't happen if moves exist)



-- Story 11 : Design simple text format that is easy for your program to read and write. It should probably be different  your "pretty show" from the first sprint.
--The input format should describe the board game in progress, and can look very similar to your internal representation. 
--For instance, each square is a 0 for blank, 1 for player 1, or 2 for player 2. The current turn or other intangible components are given in the first (or last) few lines.
--I suggest you use newlines, spaces, or other delimiters that afford the use of lines, words, and splitOn.


--allLines 0,0,H              how we represent a line, all lines is based on how big the board is
--player1 = B player2 = W     the second line will only prnt the letter to represent the current turn
--boxesWon 0,1,W 1,1,B 2,3,W  all the boxes that have been completed and who completed them seperated by a space so we can tell the program to split on spaces
--moveHistory 0,0,H 0,0,V     a list of move history

-- Story 12
readGame :: String -> Game
readGame str = 
  let input = lines str
      boardLines = parseLines (head input)
      activePlayer = parsePlayer (input !! 1)
      boxesWon = parseBoxes (input !! 2)
      moveHistory = parseMoves (input !! 3)
  in (boardLines, activePlayer, boxesWon, moveHistory)

parseLines :: String -> Board
parseLines str = 
  let lineStrs = words str
  in [parseLine line | line <- lineStrs]

parseLine :: String -> Line
parseLine str = 
  let (x:y:d:_) = splitOn "," str
  in ((read x, read y), parseDirection d)

parseDirection :: String -> Direction
parseDirection "H" = Horizontal
parseDirection "V" = Vertical
parseDirection _ = error "Invalid direction"

parsePlayer :: String -> Player
parsePlayer "B" = PlayerTwo
parsePlayer "W" = PlayerOne
parsePlayer _ = error "Invalid playyer"

parseBoxes :: String -> [Box]
parseBoxes str = 
  let boxStrs = words str
  in [parseBox box | box <- boxStrs]

parseBox :: String -> Box
parseBox str = 
  let (x:y:p:_) = splitOn "," str
      player = parsePlayer p
  in ((read x, read y), player)

parseMoves :: String -> [Move]
parseMoves str = 
  let moveStrs = words str
  in [parseLine move | move <- moveStrs]


-- Story 13
showGame :: Game -> String
showGame (board, player, boxes, moves) =
  unlines [showAllLines board, showCurrPlayer player, showBoxesWon boxes, showMoveHist moves]

-- board lines to allLines format
showAllLines :: Board -> String
showAllLines board =
  unwords (map showLine board)

-- converts line to x,y,H / x,y,V
showLine :: Line -> String
showLine ((x,y) , dir) =
  show x ++ "," ++ show y ++ "," ++ case dir of
    Horizontal -> "H"
    Vertical -> "V"

-- current player to player1 = B player2 = W format
showCurrPlayer :: Player -> String
showCurrPlayer PlayerOne = "W"
showCurrPlayer PlayerTwo = "B"

-- boxes to boxesWon format
showBoxesWon :: [Box] -> String
showBoxesWon boxes = 
  unwords (map showBox boxes)

-- converts box to x,y,W / x,y,B
showBox :: Box -> String
showBox ((x,y), player) = 
  show x ++ "," ++ show y ++ "," ++ case player of
    PlayerOne -> "W"
    PlayerTwo -> "B"

-- converts move hist to moveHist format
showMoveHist :: [Move] -> String
showMoveHist moves = 
  unwords (map showLine moves)


  -- Story 14 

-- write  game state to a file

writeGame :: Game -> FilePath -> IO ()
writeGame game filePath = do
  let gameString = showGame game
  writeFile filePath gameString


-- load game state 
loadGame :: FilePath -> IO (Maybe Game)
loadGame filePath = do
  content <- readFile filePath
  let game = readGame content
  return (Just game)


-- calculate and print best move fr player 
putBestMove :: Game -> IO ()
putBestMove game = do
  let move = bestMove game -- bestMove placeholder (not implemented yet)
  putStrLn $ "Best Move: " ++ show move
  printOutcome game move

-- helper to print outcome of a move
printOutcome :: Game -> Move -> IO ()
printOutcome game move =
  let result = gameWinner (makeMove game move)
  in case result of
       Just (Winner PlayerOne) -> putStrLn "This move forces a win for PlayerOne"
       Just (Winner PlayerTwo) -> putStrLn "This move forces a win for PlayerTwo"
       Nothing -> putStrLn "This move forces a tie"

--story 17: Rate game function
rateGame :: Game -> Rating
rateGame game@(board, currentPlayer, boxes, moveHistory) =
  let currentPlayerScore = if gamePlayer game == PlayerOne then 1 else -1
      boxScore =(length [box | box <- boxes, snd box == PlayerOne]) - (length [box | box <- boxes, snd box == PlayerTwo])
      score = currentPlayerScore + boxScore
  in score

-- story 18 (and 19)
-- looks at game state and retrns best rating and corresponding move
whoMightWin :: Game -> Int -> (Rating, Maybe Move)
whoMightWin game 0 = (rateGame game, Nothing) --base case
whoMightWin game depth
  | null validMoves = (rateGame game, Nothing) --no legal moves
  | otherwise = search validMoves Nothing (if player == PlayerOne then (minBound) else maxBound)
  where
    player = gamePlayer game
    validMoves = legalMoves game

    search :: [Move] -> Maybe Move -> Rating -> (Rating, Maybe Move)
    search [] bestMove bestRating = (bestRating, bestMove)
    search (move:moves) currentBest currentBestRating =
      let (newRating, _) = whoMightWin (makeMove game move) (depth -1)
          better = if player == PlayerOne
                   then newRating > currentBestRating
                   else newRating < currentBestRating
          newBest = if better then Just move else currentBest
          updatedBestRating = if better then newRating else currentBestRating
      in if isWinningOutcome newRating player
         then (newRating, Just move)
         else search moves newBest updatedBestRating
    
    -- helper to check if a rating is a winning outcome
    isWinningOutcome :: Rating -> Player -> Bool
    isWinningOutcome rating PlayerOne = rating == maxBound
    isWinningOutcome rating PlayerTwo = rating == minBound

readMove :: String -> Maybe Move
readMove str = 
    do 
      let (x:y:d:_) = splitOn "," str
      return ((read x, read y), parseDirection d)

winEval :: Winner -> String
winEval w = case w of
    Draw -> "Draw"
    Winner player -> "Winner:" ++ show player

showBestMove :: Game -> String
showBestMove game = show (bestMove game)

showMove :: Move -> String
showMove = show