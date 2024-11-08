type Point = (Int, Int)
type Line = (Point, Direction)
data Direction = Vertical | Horizontal deriving Eq
type Box = (Point, Player)
type Board = [Line]

data Player = PlayerOne | PlayerTwo
type Game = (Board, Player, [Box])
type Move = Line

