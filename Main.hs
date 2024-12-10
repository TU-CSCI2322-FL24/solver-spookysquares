module Main where
import System.Console.GetOpt
import SpookySquare
import System.Environment

data Flag = Help | Win | Move | Verbose | Depth String deriving (Show, Eq)


main :: IO ()
main = 
    do args <- getArgs
       let (flags, inputs, errors) = getOpt Permute options args
       putStrLn $ show (flags, inputs, errors)
       if Help `elem` flags
       then putStrLn $ usageInfo "Spooky Squares [options] [filename] Dots and Boxes." options
       else 
        do let filePath = if null inputs then "gameState1.txt" else head inputs 
           --dispatch flags (indexOfName name) (cycle $ lines contents)
           game <- loadGame filePath
           putBestMove game


dispatch :: [Flag] -> Game -> Int -> IO()
dispatch flags game depth
    | any move 



options :: [OptDescr Flag]
options = 
        [Option ['w'] ["winner"] (NoArg Win) "Print best move"
        ,Option ['d'] ["depth"] (ReqArg (Depth) "NUM") "Set the depth cutoff to NUM"
        ,Option ['h'] ["help"] (NoArg Help) "Show help message"
        ,Option ['m'] ["move"] (NoArg Move) "Make a move on game"
        ,Option ['v'] ["verbose"] (NoArg Verbose) "Show rating / result of a move"]


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

-- write  game state to a file

writeGame :: Game -> FilePath -> IO ()
writeGame game filePath = do
  let gameString = showGame game
  writeFile filePath gameString
  putStrLn $ "Game state written to " ++ filePath


-- load game state 
loadGame :: FilePath -> IO Game
loadGame filePath = do
  content <- readFile filePath
  let game = readGame content
  putStrLn "Game state loaded successfully!"
  return game