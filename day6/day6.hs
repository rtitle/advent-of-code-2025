import Data.List.Split (splitEvery, splitOn, splitWhen)
import Data.List (transpose)

toInt :: String -> Int
toInt s = read s :: Int

parseInput :: [String] -> ([[Int]], String)
parseInput lns = (nums, ops) where
  nums = fmap (fmap toInt . filter (not . null) . splitOn " ") . take ((length lns) - 1) $ lns
  ops =  concat . filter (not . null) . splitOn " " . last $ lns

part1 :: [[Int]] -> String -> Int
part1 nums ops = let t = transpose nums in sum . fmap f $ (zip t ops) where
  f (ns, op) = if op == '+' then add ns else mult ns
  add = foldl (+) 0
  mult = foldl (*) 1

part2 :: [String] -> String -> Int
part2 lns ops = res where
  t = transpose lns
  ints = fmap (\s -> if null s then -1 else toInt s) . fmap (filter (/= ' ')) $ t
  res = sum . fmap f . zip ops . splitWhen (== -1) $ ints
  f (op, ns) = if op == '+' then add ns else mult ns
  add = foldl (+) 0
  mult = foldl (*) 1

main :: IO ()
main = do 
  input <- readFile "day6-input.txt"
  let lns = lines input
  let (ints, ops) = parseInput lns
  let p1 = part1 ints ops
  print p1
  let nums = take ((length lns) - 1) lns
  let p2 = part2 nums ops
  print p2