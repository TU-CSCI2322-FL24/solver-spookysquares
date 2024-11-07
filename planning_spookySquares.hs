------------- Planning -------------

type Point = (Int, Int)
type Edge = (Point, Direction)
type Box = Point
type Board = [[Box]]

data Direction = Up | Down | Izquierda | Derecha deriving Eq

type Game = Board
type Move = Edge
type Player1 = Int
type Player2 = Int

-- Testing code --

gameTest :: Game
gameTest = [[(0,0), (1,0), (2,0)], [(0,1), (1,1), (2,1)], [(0,2), (1,2), (2,2)]]
p1Move :: Move
p1Move = ((0,0), Derecha)
p2Move :: Move
p2Move = ((0,1), Down)

makeMove :: Game -> Move -> Game
makeMove game move = undefined

--this function checks if the move exceeds the grid limits
validMove :: Game -> Move -> Bool
validMove game move =
    let point = fst move
        boardLimit = case snd move of
                    Up -> snd point /= 0
                    Down -> snd point > (snd head(last game) + 1)
                    Derecha -> fst point > (fst head(head game) + 1)
                    Izquierda -> fst point /= 0
        checkEmpty = undefined -- check if point and direction is already filled in 
    in boardLimit && 

-- Planing 12:30 --

--type Game = Board
--type Board = [[Point]]
--type Move = Edge
--
--type Point = (Int, Int)
--type Edge = (Point, Point) -- cause an edge has end points (so the edge is the stuff between the points)
--type Box = [Edge]

-- Shared Planning Doc: https://docs.google.com/document/d/14zdDI9YBC7k1rin2WbVVFhhn5QvC527lzKjWfOn1nJY/edit?tab=t.0

