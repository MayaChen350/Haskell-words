module Utils where
import System.Random
import Control.Monad (forM)
import Data.List (foldl')

-- Fisher-Yates shuffle function
shuffle :: [a] -> IO [a]
shuffle xs = do
    gen <- newStdGen
    let n = length xs
    indices <- forM [1..n] $ \i -> randomRIO (i, n)
    return $ foldl' (\acc (i, j) -> swap i j acc) xs (zip [1..n] indices)
  where
    swap i j xs = let elemI = xs !! (i - 1)
                      elemJ = xs !! (j - 1)
                  in take (i - 1) xs ++ [elemJ] ++ drop i (take (j - 1) xs) ++ [elemI] ++ drop j xs
