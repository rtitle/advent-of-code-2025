data Dir = L | R deriving (Eq, Show, Read)

parseLine :: String -> (Dir, Int)
parseLine (x:xs) = (read [x], read xs :: Int)

part1 (dir, n) (curDial, numZeros) = 
  let n' = if dir == R then n else -n
      curDial' = curDial + n'
      finalDial = curDial' `mod` 100
  in (finalDial, if finalDial == 0 then numZeros + 1 else numZeros)

part2 (dir, n) (curDial, numZeros) = 
  let n' = if dir == R then n else -n
      curDial' = curDial + n'
      finalDial = curDial' `mod` 100
      initialLeftPass = if dir == L && n >= curDial && curDial /= 0 then 1 else 0
      initialRightPass = if dir == R && n >= (100 - curDial) then 1 else 0
      remainderLeft = if dir == L && n >= curDial then (n - curDial) `div` 100 else 0
      remainderRight = if dir == R && n >= (100 - curDial) then (n - (100 - curDial)) `div` 100 else 0
      nz = numZeros + initialLeftPass + initialRightPass + remainderLeft + remainderRight
  in (finalDial, nz)

main :: IO ()
main = do 
    input <- readFile "day1-input.txt"
    let lns = lines input
    let parsed = reverse . (fmap parseLine) $ lns
    let (_, p1) = foldr part1 (50, 0) parsed
    print p1
    let (_, p2) = foldr part2 (50, 0) parsed
    print p2