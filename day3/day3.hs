import Data.Char (digitToInt, intToDigit)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

findMax :: [Int] -> Int -> Int -> Int
findMax xs start end = maximum x2 where
  x1 = drop start xs
  x2 = reverse . drop ((length xs) - end) . reverse $ x1

part1 :: [Int] -> Int
part1 xs = a * 10 + b where
  a = findMax xs 0 ((length xs) - 1)
  b = findMax xs (succ . fromJust $ elemIndex a xs) (length xs)

toInt :: String -> Int
toInt s = read s :: Int

part2 :: [Int] -> Int
part2 xs = toInt . (fmap intToDigit) . reverse $ res where
  (_, res, _) = foldl inner (11, [], 0) xs
  inner (-1, r, lastIdx) c = (-1, r, lastIdx)
  inner (n, [], lastIdx) c = let mx = findMax xs 0 ((length xs) - n) in (n - 1, [mx], fromJust $ elemIndex mx xs)
  inner (n, r, lastIdx) c = let mx = findMax xs (lastIdx + 1) ((length xs) - n) in (n - 1, mx : r, lastIdx + 1 + (fromJust $ elemIndex mx (drop (lastIdx+1) xs)))


main :: IO ()
main = do 
    input <- readFile "day3-input.txt"
    let parsed = fmap (fmap digitToInt) . lines $ input
    let p1 = sum . fmap part1 $ parsed
    print p1
    let p2 = sum . fmap part2 $ parsed
    print p2