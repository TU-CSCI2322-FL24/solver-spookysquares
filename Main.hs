module Main where

import System.Console.GetOpt
import SpookySquare
import System.Environment
import Data.Maybe
import Text.Read

data Flag = Help | Win | Move String | Verbose | Depth String deriving (Show, Eq)


main :: IO ()
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    putStrLn $ show (flags, inputs, errors)
    if Help `elem` flags
    then putStrLn $ usageInfo "Spooky Squares [options] [filename] Dots and Boxes." options
    else do
        let filePath = if null inputs then "gameState1.txt" else head inputs
        maybeGame <- loadGame filePath 
        case maybeGame of
            Nothing -> putStrLn "Error: Failed to load game."
            Just game -> case getDepth flags of
                Nothing -> putStrLn "Error: Depth not specified."
                Just depth -> dispatch flags game depth


dispatch :: [Flag] -> Game -> Int -> IO ()
dispatch flags game depth
    | any isMove flags = moveIO flags game 
    | Win `elem` flags = 
        if Verbose `elem` flags
        then putStrLn("Best move:" ++ showBestMove game ++ ", outcome:" ++  winEval (whoWillWin game))
        else putStrLn("Best move:" ++ showBestMove game)
    | otherwise = if Verbose `elem` flags then putStrLn (goodMovePrint game True)
                else putStrLn (goodMovePrint game False)


goodMovePrint:: Game -> Bool -> String
goodMovePrint game isVerbose =
    case whoMightWin game 3 of
        (rated, Just m) -> if isVerbose then "Try move: " ++ showMove m ++ "  Rating:" ++ show rated else "Try move: " ++ showMove m
        (_, Nothing) -> "..."



getDepth :: [Flag] ->  Maybe Int
getDepth [] = Just 3
getDepth (Depth d:_) = readMaybe d
getDepth (_:flags) = getDepth flags

moveIO :: [Flag] -> Game -> IO()
moveIO flags game = 
    case getMove flags game of
        Just move -> 
            let newGame = makeMove game move
            in if Verbose `elem` flags then putStrLn $ prettyPrint newGame else putStrLn $ showGame newGame 
        Nothing -> putStrLn "Error"

getMove:: [Flag] -> Game -> Maybe Move
getMove [] _ = Nothing
getMove (Move m: _) game = readMove m
getMove (_:flags) game = getMove flags game

isMove :: Flag -> Bool
isMove (Move _)= True
isMove _ = False




options :: [OptDescr Flag]
options =
        [Option ['w'] ["winner"] (NoArg Win) "Print best move"
        ,Option ['d'] ["depth"] (ReqArg (Depth) "Num") "Set the depth cutoff to NUM"
        ,Option ['h'] ["help"] (NoArg Help) "Show help message"
        ,Option ['m'] ["move"] (ReqArg (Move) "Move") "Make a move on game"
        ,Option ['v'] ["verbose"] (NoArg Verbose) "Show rating / result of a move"]