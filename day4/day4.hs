import qualified Data.Map as M

type Coord = (Int, Int)

parseInput :: [String] -> [[Bool]]
parseInput = fmap (fmap (== '@'))

part1 :: [[Bool]] -> (Int, [[Bool]])
part1 grid = (length res, newGrid) where 
  newGrid = fmap (\(y, row) -> fmap (\(x, item) -> if (x, y) `elem` res then False else item) (zip [0..] row)) (zip [0..] grid)
  res = fmap fst . (filter resultCond) . M.toList $ rows
  resultCond ((x, y), v) = if isCorner grid x y then True else if isEdge grid x y then v >= 2 else v >= 5
  rows = foldl cols M.empty (zip [0..] grid)
  cols r (y, c) = foldl inner r (zip (repeat y) (zip [0..] c))
  inner r (y, (x, True)) = if (isCorner grid x y) && (x, y) `M.notMember` r then M.insert (x, y) 1 r else r
  inner r (y, (x, False)) = foldl (\rr cc -> M.insertWith (+) cc 1 rr) r (adjacent grid x y)

part2 :: [[Bool]] -> Int
part2 grid = let (n, next) = part1 grid in if next == grid then n else n + part2 next


adjacent :: [[Bool]] -> Int -> Int -> [Coord]
adjacent grid x y = filter (\(kx, ky) -> kx >= 0 && ky >= 0 && kx < width && ky < height && grid !! ky !! kx) [(x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1), (x+1, y), (x+1, y+1), (x, y+1), (x-1, y+1)] where 
  width = length (head grid)
  height = length grid

isCorner :: [[Bool]] -> Int -> Int -> Bool
isCorner grid x y = (x == 0 && y == 0) || (x == width-1 && y == 0) || (x == width-1 && y == height-1) || (x == 0 && y == height-1) where
  width = length (head grid)
  height = length grid

isEdge :: [[Bool]] -> Int -> Int -> Bool
isEdge grid x y = x == 0 || y == 0 || x == width-1 || y == height-1 where
  width = length (head grid)
  height = length grid

main :: IO ()
main = do 
    input <- readFile "day4-input.txt"
    let parsed = parseInput (lines input)
    let (p1, _) = part1 parsed
    print p1
    let p2 = part2 parsed
    print p2
