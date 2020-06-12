module Tes (
    
) where
import System.Random (newStdGen, split)
import System.Random.Shuffle (shuffle')


-- rand :: [a] -> IO [a]
rand mineNumber game = do
    rng <- newStdGen 
    return $ randomMines mineNumber game rng

-- randCoor = zip (rand [1,2,3,4]) (rand [1,2,3,4])
randomMines mineNumber game gen =
    let grid = game
        height = length grid
        width = length (grid !! 0)

        (gen1, gen2) = split gen
        xArray = [0..width-1]
        yArray = [0..height-1]

        xArrayShuffled = take mineNumber $ shuffle' xArray (length xArray) gen1
        yArrayShuffled = take mineNumber $ shuffle' yArray (length yArray) gen2
    in zip xArrayShuffled yArrayShuffled

