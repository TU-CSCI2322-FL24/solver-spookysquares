type Point = (Int, Int)
type Line = (Point, Direction)
data Direction = Vertical | Horizontal deriving (Eq, Show)
type Box = (Point, Player)
type Board = [Line]

-- Game type is a 4-tuple containing the complete board, the active player, the completed boxes, and the history of moves
type Game = (Board, Player, [Box], [Move])
data Player = PlayerOne | PlayerTwo deriving (Show)
type Move = Line
type Winner = Player

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
gameStateMoves = [((0,0), Horizontal), ((3,0), Horizontal), ((0,1), Horizontal), ((0,2), Horizontal), ((1,1), Horizontal), ((1,2), Horizontal), ((2,3), Horizontal), ((0,0), Vertical), ((0,1), Vertical), ((1,0), Vertical), ((1,1), Vertical), ((1,2), Vertical), ((2,2), Vertical), ((3,2), Vertical)]
gameStateExample :: Game
gameStateExample = (allLines, PlayerOne, gameStateBoxes, gameStateMoves)

-- to calc all lines in given box. input box of ((0,0), Player1)
-- TO DO : When checking if a box is Valid you only need to check first line and the vertical of that one Ex. check (0,0, Horizontal), (0,0, Vertical)
calcBox :: Box -> [Line]
calcBox box =
    let point = fst box
        horPoint = (fst point + 1, snd point)
        vertPoint = (fst point, snd point + 1)
    in [boxLines | boxLines <- allLines, (point, Horizontal) == boxLines || (point, Vertical) == boxLines || (horPoint, Horizontal) == boxLines || (vertPoint, Vertical) == boxLines]

-- Story 2 : Determine who has won the game. Write a function Game -> Winner
-- Ana, Adrian


-- Story 3 : Compute the result of making a legal move in a game state, write a function of type
-- Game -> Move -> Game
-- Emma


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


-- Story 6 : All functions should consider possible errors or edge cases
