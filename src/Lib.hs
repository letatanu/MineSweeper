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
    , makeRandomMine
    , setMine
    , cellCoord
    , setMines
    , createSolution
    ) where

import System.Random
import qualified Data.Map as M
-- import System.Random.Shuffle (shuffle')

data Game = Game {
                gameGrid :: Grid Cell,
                gameGridSolution :: Grid Cell,
                gameScore :: Int
                }

data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)
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

cell2char :: Cell -> Char
cell2char (Cell _ c) = c


------- 
-- createGame :: Int -> Int -> [[Char]]
createGame x y = 
    let grid = take x $ repeat $ take y $ repeat ' '
        gwc = gridWithCoords grid
    in Game gwc gwc 0 

makeRandomMine g game =
    let grid = gameGrid game
        gen = mkStdGen g
        (gen1, gen2) = split gen
        [height] = take 1 $ randomRs(0, length grid) gen1
        [width] = take 1 $ randomRs (0, length (grid !! 0)) gen2 
    in (height, width)

formatGameGrid :: Game -> Grid Char
formatGameGrid game =
    let grid = gameGrid game
        charGrid = mapOverGrid cell2char grid
    in charGrid
        
-- randomMines mineNumbers game gen =
--     let grid = gameGrid game
--         height = length grid
--         width = length (grid !! 0)

--         (gen1, gen2) = split gen
--         xArray = [0..width-1]
--         yArray = [0..height-1]

--         xArrayShuffled = shuffle' xArray (length xArray) gen1
--         yArrayShuffled = shuffle' yArray (length yArray) gen2
--     in zip xArrayShuffled yArrayShuffled

cellCoord :: Cell -> (Integer, Integer)
cellCoord (Cell b _) = b

setMine :: Game -> (Integer, Integer) -> Grid Char
setMine game loc =
    let grid = gameGrid game
        formatCell cell =
            let char = cell2char cell
                coordCell = cellCoord cell
            in if coordCell == loc then '*' else char
        charGrid = mapOverGrid formatCell grid
    in charGrid

setMines :: Game -> [(Integer, Integer)] -> Game
setMines game (x:xs) =
    let a = gridWithCoords $ setMine game x
        newGame = Game a a 0
    in setMines newGame xs
setMines game _ = game

---------- create solution grid

z=zip[1..]

x % i = [a | (j,a)<-z x, abs(i-j)<2]

f x=[[ head $ [c | c > ' '] 
            ++ show ( sum[1 | '*' <- (%j) =<< x%i ] ) 
            | (j,c)<-z r ] |(i,r)<-z x ]

createSolution game = 
    let grid = formatGameGrid game
    in f grid


playGame 