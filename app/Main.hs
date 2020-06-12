module Main where
import System.Random
import Lib
import Data.List.Split

main :: IO ()
main = do
    (x,y) <- correctFormat "Please enter a size of grid. It should look like -- height width --..."
    mineNumber <- checkMineNumber $ x * y
    gen <- newStdGen
    let newGame = createGame x y mineNumber
        mineLocations = randomMines (toInteger mineNumber) newGame gen
        game = setMines newGame mineLocations
        generatedGame = createSolution game
    playTurn generatedGame

playTurn game = do
    let barLine = take 20 $ repeat "-"
    putStrLn $ concat barLine
    showGrid . formatGameGrid $ game
    (x,y) <- correctFormat "Please enter your choice, should be -- x y -- "
    let newGame = playGame game (x,y)
        (complete, win) = completed newGame
    case complete of 
        True -> do
            case win of 
                True -> do 
                    showGrid . formatGameGridSolution $ game
                    putStrLn "You won! Want to do another game? Y/N"
                    input <- getLine
                    case input of  
                        "Y" -> main
                        _ -> return()
                _ -> do
                    showGrid . formatGameGridSolution $ game
                    putStrLn "Sorry to see you lose! Want to do another game? Y/N"
                    input <- getLine
                    case input of  
                        "Y" -> main
                        _ -> return()
        _ -> playTurn newGame

correctFormat mssg= do
    putStrLn $ mssg
    -- hFlush stdout
    input <- getLine
    let inputs = splitOn " " input
    case (length inputs) of 
            2 -> do
                let x = readMaybe (inputs !! 0) :: Maybe Int
                    y = readMaybe (inputs !! 1) :: Maybe Int
                case (x, y) of
                    (Just c, Just d) -> return (c,d) 
                    _ -> do
                        putStrLn "Invalid input."
                        correctFormat mssg
            _ -> do
                putStrLn "Invalid input."
                correctFormat mssg

-- readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

checkMineNumber size = do
    putStrLn "Please enter the number of mines ..."
    mineNumber <- getLine
    let x = readMaybe mineNumber :: Maybe Int
    case x of
        Just a -> if (a < size )
            then return a 
            else do
                putStrLn "Invalid number of mines."
                checkMineNumber size
        _ -> do
                putStrLn "Invalid number of mines."
                checkMineNumber size
