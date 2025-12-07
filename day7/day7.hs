import Control.Monad.RWS (RWS, evalRWS, modify, get, put)
import Data.List (nub)
import qualified Data.Map as M

indexesOf :: Eq a => a -> [a] -> [Int]
indexesOf a as = foldr f [] (zip [0..] as) where
  f (i, c) r = if c == a then i:r else r
  

part1 :: [String] -> [Int] -> Int
part1 [] _ = 0
part1 (x:xs) beams = count + part1 xs (nub newBeams) where
  splitters = indexesOf '^' x
  (newBeams, count) = foldl f ([], 0) beams
  f (r, cnt) c = if c `elem` splitters then ((filter (/= c) r) ++ [c-1, c+1], cnt+1) else (r ++ [c], cnt)


part2 :: [String] -> Int -> Int
part2 lns initialBeam = fst $ evalRWS (inner (zip [0..] lns) initialBeam) () M.empty where
  inner :: [(Int, String)] -> Int -> RWS () () (M.Map (Int, Int) Int) Int
  inner [] _ = return 0
  inner ((i, x):xs) beam = 
    do
        let splitters = indexesOf '^' x
        let isSplit = beam `elem` splitters
        seen <- get

        let go b = if (i+1, b) `M.member` seen 
            then do
                let next = seen M.! (i+1, b)
                put (M.insert (i, b) next seen)
                return next
            else do
                next <- inner xs b
                modify (M.insert (i, b) next)
                return next

        if isSplit then fmap ((+1) . sum) $ (traverse go [beam-1, beam+1]) else go beam


main :: IO ()
main = do
  input <- readFile "day7-input.txt"
  let (start:lns) = lines input
  let startIdx = indexesOf 'S' start
  let p1 = part1 lns startIdx
  print p1
  let p2 = (part2 lns (head startIdx)) + 1  --add the current reality
  print p2