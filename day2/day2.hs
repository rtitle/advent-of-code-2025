import Data.List.Split
import Data.List (nub)


toInt :: String -> Int
toInt s = read s :: Int


-- part 1 --
findLowerBound :: String -> Int
findLowerBound s = 
  let len = length s 
      odd = 10 ^ ((len - 1) `div` 2)
      even = let x = take (len `div` 2) s in if toInt (x ++ x) < toInt s then (toInt x) + 1 else toInt x
  in if len `mod` 2 == 1 then odd else even

findUpperBound :: String -> Int
findUpperBound s =
  let len = length s
      odd = 10 ^ ((len - 1) `div` 2) - 1
      even = let x = take (len `div` 2) s in if toInt (x ++ x) > toInt s then (toInt x) - 1 else toInt x
  in if len `mod` 2 == 1 then odd else even

combine :: [Int] -> [Int] -> Int
combine lbs ubs = foldl fn 0 (zip lbs ubs) where
  fn r (lower, upper) = r + combine1 lower upper

combine1 :: Int -> Int -> Int
combine1 start upper = if start > upper then 0 else double + combine1 (start+1) upper where
  double = read ((show start) ++ (show start)) :: Int
 

-- part 2 -- 
combine2 :: [String] -> Int
combine2 [lower, upper] = sum . nub $ (fn initCur initLen)
  where
    initCur = 1
    initLen = 2
    lowerInt = toInt lower
    upperInt = toInt upper
    fn cur len = let rpt = toInt . concat . (take len) . repeat . show $ cur in
        if len == initLen && rpt > upperInt then []
        else if rpt < lowerInt then fn cur (len+1)
        else if rpt >= lowerInt && rpt <= upperInt then [rpt] ++ (fn cur (len+1))
        else fn (cur+1) initLen
combine2 _ = 0


main :: IO ()
main = do 
    input <- readFile "day2-input.txt"
    let s1 = splitOn "," input
    let ranges = fmap (splitOn "-") s1
    let lbs = fmap (findLowerBound . head) ranges
    let ubs = fmap (findUpperBound . head . reverse) ranges
    let part1 = combine lbs ubs
    print part1
    let part2 = sum (fmap combine2 ranges)
    print part2