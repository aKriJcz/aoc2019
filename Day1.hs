import Data.Function (fix)

main = part2

loadMasses filename = readFile filename >>= return . fmap read . lines

fuel mass = max ((floor (fromIntegral mass / 3)) - 2) 0

fuelfuel = fix (\rec f -> if f > 0 then f + rec (fuel f) else f)

part1 = loadMasses "day1-input" >>= print . sum . fmap fuel

part2 = loadMasses "day1-input" >>= print . sum . fmap (fuelfuel . fuel)
