module Data (
    grid
    , f
    , earlyChristmas
) where
import System.Console.ANSI

grid = [ "   *    " ,"   **   " ,"        "]

z=zip[1..]

x % i =
    [a | (j,a)<-z x, abs(i-j)<2]

f x=[
    [ head $ [c | c > ' '] 
            ++ show ( sum[1 | '*' <- (%j) =<< x%i ] ) 
            | (j,c)<-z r ]
    |(i,r)<-z x ]


earlyChristmas :: String -> IO ()
earlyChristmas s = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red
        --  , SetColor Background Dull Green
         ]
  putStr s
  setSGR [ Reset ]