{-# LANGUAGE BlockArguments #-}
module Main where
import System.Console.GetOpt
import SpookySquare
import System.Environment

data Flag = Help | Win | Move | Verbose | Depth String deriving (Show, Eq)


main :: IO ()
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    putStrLn $ show (flags, inputs, errors)
    if Help `elem` flags
    then putStrLn $ usageInfo "Spooky Squares [options] [filename] Dots and Boxes." options
    else do
        let filePath = if null inputs then "gameState1.txt" else head inputs
        maybeGame <- loadGame filePath -- 'maybeGame' has type Maybe Game
        case maybeGame of
            Nothing -> putStrLn "Error: Failed to load game."
            Just game -> case getDepth flags of
                Nothing -> putStrLn "Error: Depth not specified."
                Just depth -> dispatch flags game depth


dispatch :: [Flag] -> Game -> Int -> IO ()


getDepth :: [Flag] ->  Int





options :: [OptDescr Flag]
options =
        [Option ['w'] ["winner"] (NoArg Win) "Print best move"
        ,Option ['d'] ["depth"] (ReqArg (Depth) "Num") "Set the depth cutoff to NUM"
        ,Option ['h'] ["help"] (NoArg Help) "Show help message"
        ,Option ['m'] ["move"] (ReqArg (MoveInput) "Move") "Make a move on game"
        ,Option ['v'] ["verbose"] (NoArg Verbose) "Show rating / result of a move"]