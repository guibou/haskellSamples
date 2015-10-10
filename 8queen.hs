import System.Environment
import Data.List (permutations)

main :: IO ()
main = do
  n <- (read . head) <$> getArgs
  print $ length (queens n)

generate :: Int -> [[Int]]
generate n = permutations [1..n]

sameDiag :: Int -> [Int] -> Bool
sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

queens :: Int -> [[Int]]
queens n = filter test (generate n)
    where test []         = True
          test (q:qs)     = isSafe q qs && test qs
          isSafe   try qs = not (sameDiag try qs)

{-
Times:

n res t
1 1   0.001
2 0   0.001
3 0   0.001
4 2   0.001
5 10  0.001
6 4   0.001
7 40  0.002
8 92  0.014
9 352 0.116
10 724 1.267
11 2680 14.413
12 --> killed after 2 minutes (but no ram excess)
-}
