module Lib
    ( 
    showGrid
    , addingBars
    , addingBar
    , formatGrid
    , gridWithCoords
    , cell2char
    , createGame
    , formatGameGrid
    , randomMines
    , setMine
    , cellCoord
    , setMines
    , createSolution
    , cellSolutionAtLocation
    , replace
    , Cell(Cell, Empty)
    , Game (gameGrid, gameGridSolution, gameScore)
    , formatGameGridSolution
    , playGame
    ) where

import System.Random
import qualified Data.Map as M
import System.Random.Shuffle (shuffle')

data Game = Game {
                gameGrid :: Grid Cell,
                gameGridSolution :: Grid Cell,
                gameScore :: Int
                }

data Cell = Cell (Integer, Integer) Char 
          | Empty deriving (Eq, Ord, Show)

type Grid a = [[a]]


-- This is for showing the grid to terminal --

showGrid :: [[Char]] -> IO()
showGrid = putStr . unlines . addingBars . formatGrid

formatGrid :: [[Char]] -> [String]
formatGrid (x:xs) = 
    let line = formatLine x
    in line : (formatGrid xs)
formatGrid _ = []

formatLine :: [Char] -> String
formatLine (x:xs) = 
        let a = "| " ++ [x]
        in a ++ formatLine xs
formatLine _ = "|" 

addingBars:: [[Char]] -> [String]
addingBars (x:xs)= 
    let a = addingBar x
    in a ++ (addingBars xs)
addingBars [] = []

addingBar :: [Char] -> [String]
addingBar x = 
    let bar = take (length x) (repeat '.')
    in [bar] ++ [x]

-- ---
zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip


zipOverGridWith :: (a-> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
    let rows = map repeat [0..]
        cols = repeat [0 ..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

-- formatGrid :: Grid Cell -> String
-- formatGrid = unlines . mapOverGrid cell2char 




------- 
-- createGame :: Int -> Int -> [[Char]]
createGame x y = 
    let grid = take x $ repeat $ take y $ repeat ' '
        gwc = gridWithCoords grid
    in Game gwc gwc 0 




-- randCoor = zip (rand [1,2,3,4]) (rand [1,2,3,4])
randomMines :: RandomGen b => Integer -> Game -> b -> [(Integer, Integer)]
randomMines mineNumber game gen =
    let grid = formatGameGrid game
        height = length grid
        width = length (grid !! 0)

        (gen1, gen2) = split gen
        xArray = [(0::Integer).. (toInteger (width-1))]
        yArray = [(0::Integer)..(toInteger (height-1))]

        xArrayShuffled = take (fromIntegral mineNumber) $ shuffle' xArray (length xArray) gen1
        yArrayShuffled = take (fromIntegral mineNumber) $ shuffle' yArray (length yArray) gen2
    in zip xArrayShuffled yArrayShuffled

formatGameGrid :: Game -> Grid Char
formatGameGrid game =
    let grid = gameGrid game
        charGrid = mapOverGrid cell2char grid
    in charGrid

formatGameGridSolution :: Game -> Grid Char
formatGameGridSolution game =
    let grid = gameGridSolution game
        charGrid = mapOverGrid cell2char grid
    in charGrid

cellCoord :: Cell -> (Integer, Integer)
cellCoord (Cell b _) = b
cellCoord Empty = (-1,-1)

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Empty = '-'

setMine :: Game -> (Integer, Integer) -> Grid Char
setMine game loc =
    let grid = gameGridSolution game
        formatCell cell =
            let char = cell2char cell
                coordCell = cellCoord cell
            in if coordCell == loc then '*' else char
        charGrid = mapOverGrid formatCell grid
    in charGrid

setMines :: Game -> [(Integer, Integer)] -> Game
setMines game (x:xs) =
    let a = gridWithCoords $ setMine game x
        newGame = game {
            gameGridSolution = a
        }
    in setMines newGame xs
setMines game _ = game

---------- create solution grid

z=zip[1..]

x % i = [a | (j,a)<-z x, abs(i-j)<2]

f x=[[ head $ [c | c > ' '] 
            ++ show ( sum[1 | '*' <- (%j) =<< x%i ] ) 
            | (j,c)<-z r ] |(i,r)<-z x ]

createSolution game = 
    let grid = formatGameGridSolution game
        newGame = game {
            gameGridSolution = gridWithCoords (f grid)
        }
    in newGame

cellSolutionAtLocation :: Game -> (Int, Int) -> Cell
cellSolutionAtLocation game loc =
    let gridSolution = gameGridSolution game
        (x, y) = loc
        height = length gridSolution
        width = length $ gridSolution !! 0
    in if (x < height && y < width ) 
        then ((gridSolution !! x) !! y)
        else Empty

cellAtLocation game loc =
    let grid = gameGrid game
        (x, y) = loc
        height = length grid
        width = length $ grid !! 0
    in if (x < height && y < width ) 
        then ((grid !! x) !! y)
        else Empty 

replace _ _ [] = []
replace oldVal newVal (x:xs)
   | x == oldVal = newVal:xs
   | otherwise = x:replace oldVal newVal xs

playGame game input
    | cellAtLocation game input == Empty = game
    | otherwise =
        let gridSolution = gameGridSolution game
            grid = gameGrid game
            cell = cellAtLocation game input
            solutionCell = cellSolutionAtLocation game input
            newgame = case solutionCell of
                Empty -> game
                otherwise ->  
                    let (x,y) = cellCoord cell
                        newGridRow = replace cell solutionCell (grid !! (fromIntegral x))
                        newGrid = replace (grid !! (fromIntegral x)) newGridRow grid
                        score = (gameScore game) + 1
                    in game {
                        gameGrid = newGrid,
                        gameScore = score
                    }
        in newgame