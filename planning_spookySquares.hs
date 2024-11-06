------------- Planning -------------

type Point = (Int, Int)
type Edge = (Point, Point)
type Box = ([Edge], Int)
type Board = [[Box]]

type Game = Board
type Move = Edge
type playerTurn = Bool
