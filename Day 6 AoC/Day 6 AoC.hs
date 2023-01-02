import System.IO

takeCharOut :: [(Int, Char)] -> [Char]
takeCharOut x = [ c | (i, c) <- x]

fourteenChar [_] = [False]
fourteenChar (x:xs) = [ x==b | b <- xs] ++ fourteenChar xs
fourteenCharH x = True `elem` fourteenChar x

siphen1 :: [(Int, Char)] -> (Int, Char)
siphen1 (a:xs)
 | fourteenCharH (takeCharOut (a:take 3 xs)) = siphen1 xs
 | otherwise = xs!!2

tuple1 (a, b) = a

siphen2 :: [(Int, Char)] -> (Int, Char)
siphen2 (x:xs)
 | fourteenCharH (takeCharOut(x : take 13 xs)) = siphen2 xs
 | otherwise = xs!!12

main = do
  x <- readFile "input.txt"
  print (fourCharF ['a', 'b', 'c', 'c'])
  print(tuple1 (siphen1 (zip [1..] x)))
  print(tuple1 (siphen2 (zip [1..] x)))
