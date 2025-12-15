import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.RWS (RWS, evalRWS, modify, get, put, tell)

parseInput :: [String] -> M.Map String [String]
parseInput = M.fromList . fmap parseLine where
    parseLine s = let [k, v] = splitOn ": " s in (k, splitOn " " v)

part1 :: M.Map String [String] -> Int
part1 g = sum . snd $ evalRWS (inner "you") () S.empty where
    inner :: String -> RWS () [Int] (S.Set String) ()
    inner "out" = tell [1]
    inner node = do
        modify (S.insert node)
        visited <- get
        let next = filter (`S.notMember` visited) (g M.! node)
        traverse inner next
        modify (S.delete node)
        return ()

part2 :: M.Map String [String] -> String -> String -> Int
part2 g start end = fst $ evalRWS (inner start) () (S.empty, M.empty) where
    inner :: String -> RWS () () ((S.Set String), (M.Map String Int)) Int
    inner node = if node == end then return 1 else do
        modify (\(a,b) -> ((S.insert node a), b))
        (visitedThisBranch, visitedCache) <- get
        let nextNodes = if node `M.member` g then filter (`S.notMember` visitedThisBranch) (g M.! node) else []
        let go nextNode = case M.lookup nextNode visitedCache of
                              Just cached -> return cached
                              Nothing -> do
                                  nextRes <- inner nextNode
                                  modify (\(a, b) -> (a, (M.insert nextNode nextRes b)))
                                  return nextRes
        res <- traverse go nextNodes
        modify (\(a,b) -> ((S.delete node a), b))
        return (sum res)

main :: IO ()
main = do
    input1 <- readFile "day11-input.txt"
    let graph1 = parseInput (lines input1)
    let p1 = part1 graph1
    print p1
    input2 <- readFile "day11-input.txt"
    let graph2 = parseInput (lines input2)
    let dacToOut = part2 graph2 "dac" "out"
    let fftToDac = part2 graph2 "fft" "dac"
    let svrToFft = part2 graph2 "svr" "fft"
    let p2 = dacToOut * fftToDac * svrToFft
    print p2