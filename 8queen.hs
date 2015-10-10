import System.Environment

main :: IO ()
main = do
  n <- (read . head) <$> getArgs
  print $ length (queens n)

queens :: Int -> [[Int]]
queens n = filter test (generate n)
    where generate 0      = [[]]
          generate k      = [q : qs | q <- [1..n], qs <- generate (k-1)]
          test []         = True
          test (q:qs)     = isSafe q qs && test qs
          isSafe   try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

{-
Times:

n res t
1 1   0.001
2 0   0.001
3 0   0.001
4 2   0.001
5 10  0.001
6 4   0.007
7 40  0.1
8 92  2.554
9 ----------> Unknown (stoped after 2 minutes and 10 Gb of ram)
-}
