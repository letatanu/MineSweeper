module Main where
import System.Random
import Lib
import Data
import Data.List.Split

main :: IO ()
main = do
    putStrLn "Please enter a size of grid. It should look like -- height width --..."
    size <- getLine
    putStrLn "Please enter the number of mines ..."
    mineNumber <- getLine
    gen <- newStdGen
    let inputs = splitOn " " size
        x = read (inputs !! 0) :: Int
        y = read (inputs !! 1) :: Int
        newGame = createGame x y
        mineLocations = randomMines (read mineNumber :: Integer) newGame gen
        game = setMines newGame mineLocations
        generatedGame = createSolution game
    playTurn generatedGame

playTurn game = do
    showGrid . formatGameGrid $ game
    putStr "Please enter your choice, should be x y "
    loc <- getLine
    let inputs = splitOn " " loc
        x = read (inputs !! 0) :: Int
        y = read (inputs !! 1) :: Int
        newGame = playGame game (x,y)
    playTurn newGame

    