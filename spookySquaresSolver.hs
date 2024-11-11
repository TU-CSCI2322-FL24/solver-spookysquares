type Point = (Int, Int)
type Line = (Point, Direction)
data Direction = Vertical | Horizontal deriving Eq
type Box = (Point, Player)
type Board = [Line]

data Player = PlayerOne | PlayerTwo
type Game = (Board, Player, [Box])
type Move = Line


-- Story 2 : Determine who has won the game. Write a function Game -> Winner



-- Story 3 : Compute the result of making a legal move in a game state, write a function of type
-- Game -> Move -> Game



-- Story 4 : Compute the legal moves from a game state, use a function Game -> [Move]



-- Story 5 : Pretty-print a game into a string, create a function Game -> String
-- you should NOT override the "Show" typeclass. 



-- Story 6 : All functions should consider possible errors or edge cases