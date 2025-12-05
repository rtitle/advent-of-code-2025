import Data.List (sort)
import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

parseRanges :: [String] -> [(Int, Int)]
parseRanges = fmap parseRange where
  parseRange s = let [a, b] = fmap toInt (splitOn "-" s) in (a, b)

parseIds :: [String] -> [Int]
parseIds = fmap toInt

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = (foldl f []) . sort where
  f [] (a, b) = [(a, b)]
  f r (a, b) = 
    let ((la, lb), len) = (last r, length r)
    in if a <= lb then take (len - 1) r ++ [(la, (max lb b))] else r ++ [(a, b)]

part1 :: [(Int, Int)] -> [Int] -> Int
part1 ranges ingredients = foldl f 0 ingredients where
  f r c = r + isFresh c
  isFresh ingredient = 
    let fil = filter (\(a, b) -> a <= ingredient && b >= ingredient) ranges
    in if null fil then 0 else 1

part2 :: [(Int, Int)] -> Int
part2 = foldl f 0 where
  f r (a, b) = r + (b - a) + 1

main :: IO ()
main = do
  input <- readFile "day5-input.txt"
  let parsed = splitWhen null (lines input)
  let ranges = mergeRanges . parseRanges . head $ parsed
  let ingredients = parseIds . head . drop 1 $ parsed
  let p1 = part1 ranges ingredients
  print p1
  let p2 = part2 ranges
  print p2
