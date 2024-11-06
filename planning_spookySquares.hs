------------- Planning -------------

type Point = (Int, Int)
type Edge = (Point, Int) -- change type
type Box = Point
type Board = [[Box]] -- change

type Game = Board
type Move = Edge
type Player1 = Int
type Player2 = Int


-- Planing 12:30 --

type Game = Board
type Board = [[Point]]
type Move = Edge

type Point = (Int, Int)
type Edge = (Point, Point) -- cause an edge has end points (so the edge is the stuff between the points)
type Box = [Edge]

-- Shared Planning Doc: https://docs.google.com/document/d/14zdDI9YBC7k1rin2WbVVFhhn5QvC527lzKjWfOn1nJY/edit?tab=t.0
