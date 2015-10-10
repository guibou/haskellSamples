import System.Environment

main :: IO ()
main = do
  n <- (read . head) <$> getArgs
  print $ length (queens n)

sameDiag :: Int -> [Int] -> Bool
sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

queens :: Int -> [[Int]]
queens n = queens' n n

queens' :: Int -> Int -> [[Int]]
queens' _ 0 = [[]]
queens' nTotal n = do
  current <- queens' nTotal (n - 1)
  col <- nextColumn current nTotal

  return $ col:current

nextColumn :: [Int] -> Int -> [Int]
nextColumn current nTotal = filter (not . (\x -> elem x current || sameDiag x current)) [1..nTotal]


{-
Times:

n res t
1 1   0.001
2 0   0.001
3 0   0.001
4 2   0.001
5 10  0.001
6 4   0.001
7 40  0.001
8 92  0.003
9 352 0.007
10 724 0.028
11 2680 0.142
12 14200 0.877
13 73712 5.276
14 365596 34.633
-}
