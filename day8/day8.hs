import Data.List (sortBy, tails, find, nub)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

type Coord = (Int, Int, Int)
type Edge = (Int, (Coord, Coord))

toInt :: String -> Int
toInt s = read s :: Int

myFst :: Coord -> Int
myFst (a, b, c) = a

parseInput :: [String] -> [Coord]
parseInput = fmap (arrToCoord . parse) where
    parse = fmap toInt . splitOn ","
    arrToCoord [x, y, z] = (x, y, z)

dist :: Coord -> Coord -> Int
dist (x1, y1, z1) (x2, y2, z2) = (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 

edges :: [Coord] -> [Edge]
edges cs = fmap (\(a, b) -> (dist a b, (a, b))) pairs where
    pairs = [(x,y) | (x:ys) <- tails cs, y <- ys]

part1 :: [Coord] -> Int
part1 coords = res where
    es = fmap snd . sortBy (comparing fst) . edges $ coords
    combos = take 1000 . fmap (\(a,b) -> [a,b]) $ es
    merged = reverse . sortBy (comparing length) . foldl f [] $ combos
    f r [ca, cb] = case ((findContained ca r), (findContained cb r)) of
        (Just a, Just b) -> (filter (\x -> x /= a && x /= b) r) ++ [nub (a ++ b)]
        (Just a, Nothing) -> (filter (\x -> x /= a) r) ++ [nub (a ++ [cb])]
        (Nothing, Just b) -> (filter (\x -> x /= b) r) ++ [nub (b ++ [ca])]
        (Nothing, Nothing) -> r ++ [[ca, cb]]
    findContained a r = find (\x -> a `elem` x) r
    res = product . fmap length . take 3 $ merged

part2 :: [Coord] -> Maybe Int
part2 coords = res where
    es = fmap snd . sortBy (comparing fst) . edges $ coords
    combos = fmap (\(a,b) -> [a,b]) $ es
    res = fst . foldl f (Nothing, []) $ combos
    f (Just x, r) _ = (Just x, r)
    f (Nothing, r) [ca, cb] = let next = getNext ca cb r in if allContained next then (Just ((myFst ca) * (myFst cb)), next) else (Nothing, next)
    allContained (r:[]) = length r == length coords
    allContained r = False
    findContained a r = find (\x -> a `elem` x) r
    getNext ca cb r = case ((findContained ca r), (findContained cb r)) of
        (Just a, Just b) -> (filter (\x -> x /= a && x /= b) r) ++ [nub (a ++ b)]
        (Just a, Nothing) -> (filter (\x -> x /= a) r) ++ [nub (a ++ [cb])]
        (Nothing, Just b) -> (filter (\x -> x /= b) r) ++ [nub (b ++ [ca])]
        (Nothing, Nothing) -> r ++ [[ca, cb]]

main :: IO ()
main = do
    input <- readFile "day8-input.txt"
    let coords = parseInput (lines input)
    let p1 = part1 coords
    print p1
    let p2 = part2 coords
    print p2
