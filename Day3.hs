import Data.List
import Control.Arrow

type Point = (Int,Int)
type Vec = (Int,Int)

data SearchState = S {
    found :: [(Point,Int,Int)]
  , tmpLen1 :: Int
  , tmpLen2 :: Int
} deriving (Show)

main = part2

part1 = do
    (line1:line2:[]) <- loadData "day3-input"
    print $ snd $ head $ sortBy (\(_,d1) (_,d2) -> compare d1 d2) $ map (id &&& dist (0,0)) $ intersections line1 line2

part2 = do
    (line1:line2:[]) <- loadData "day3-input"
    print $ intersectionsWithDistances line1 line2 (S {found=[], tmpLen1 = 0, tmpLen2 = 0})
    print $ snd $ head $ sortBy (\(_,d1) (_,d2) -> compare d1 d2) $ map (id &&& (dist (0,0) . tfst)) $ found $ intersectionsWithDistances line1 line2 (S {found=[], tmpLen1 = 0, tmpLen2 = 0})

tfst (f,_,_) = f

x = fst
y = snd

minus :: Point -> Point -> Vec
(x1,y1) `minus` (x2,y2) = (x1-x2,y1-y2)

mtail (x:xs) = xs
mtail _ = []

shead (x:xs) = Just x
shead _      = Nothing

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn spl str = let splitted = span (\c -> c /= spl) str
                  in  fst splitted : splitOn spl (mtail $ snd splitted)

loadData :: FilePath -> IO [[Point]]
loadData fileName = do
    tokens <- readFile fileName >>= return . (fmap $ splitOn ',') . lines
    return $ map (reverse . load [(0,0)]) tokens
    where
      load :: [Point] -> [String] -> [Point]
      load acc ((direction:c):ts) = let
        count = read c :: Int
        prev = head acc
        in do
          case direction of
            'U' -> load ((fst prev, snd prev + count):acc) ts
            'R' -> load ((fst prev + count, snd prev):acc) ts
            'D' -> load ((fst prev, snd prev - count):acc) ts
            'L' -> load ((fst prev - count, snd prev):acc) ts
      load acc _ = acc

intersections :: [Point] -> [Point] -> [Point]
intersections (p1s0@(p11g:p12g:p1s)) (p21g:p22g:p2s) =
    intersections' p11g p12g p21g p22g (p12g:p1s) (p22g:p2s) []
    where
      intersections' :: Point -> Point -> Point -> Point -> [Point] -> [Point] -> [Point] -> [Point]
      intersections' p11 p12 p21 p22 (p13:p1s) p2s acc = let
          isect = cross p11 p12 p21 p22
        in case isect of
             Just s -> intersections' p12 p13 p21 p22 p1s p2s (s:acc)
             Nothing -> intersections' p12 p13 p21 p22 p1s p2s (acc)
      intersections' _ _ p21 p22 _ (p23:p2s) acc = intersections' p11g p12g p22 p23 p1s0 p2s acc
      intersections' _ _ _ _ _ _ acc = acc

intersectionsWithDistances :: [Point] -> [Point] -> SearchState -> SearchState
intersectionsWithDistances (p1s0@(p11g:p12g:p1s)) (p21g:p22g:p2s) start =
    intersections' p11g p12g p21g p22g (p12g:p1s) (p22g:p2s) start
    where
      intersections' :: Point -> Point -> Point -> Point -> [Point] -> [Point] -> SearchState -> SearchState
      intersections' _ _ _ _ _ [] acc = acc
      intersections' p11 p12 p21 p22 p1s p2s acc@(S {found=found,tmpLen1=len1,tmpLen2=len2}) = let
          isect = cross p11 p12 p21 p22
          p13   = shead p1s
        in case isect of
             Just s -> let
                 distL1 = dist p11 s
               in case p13 of
                    Just p  -> intersections' p12 p p21 p22 (mtail p1s) p2s (acc {found=(s,len1+distL1,0):found,tmpLen1=0})
                    Nothing -> intersectionsWithDistances p1s0 (mtail p2s) acc
             Nothing -> case p13 of
                          Just p  -> intersections' p12 p p21 p22 (mtail p1s) p2s (acc {found=found})
                          Nothing -> intersectionsWithDistances p1s0 (mtail p2s) acc
intersectionsWithDistances _ _ s = s

cross p11 p12 p21 p22 = let
    isL1Vertical = y p11 /= y p12
    isL2Vertical = y p21 /= y p22
    cr
      | isL1Vertical == isL2Vertical = Nothing -- Obě vertikální nebo obě horizontální
      | isL1Vertical                 = let sc = (x p11, y p21) -- Suspected cross
                                       in if ((y p11 > y sc) /= (y p12 >= y sc)) && ((x p21 > x sc) /= (x p22 >= x sc)) then Just sc
                                                                                                                        else Nothing
      |                 isL2Vertical = let sc = (x p21, y p11) -- Suspected cross
                                       in if ((x p11 > x sc) /= (x p12 >= x sc)) && ((y p21 > y sc) /= (y p22 >= y sc)) then Just sc
                                                                                                                        else Nothing
      | otherwise                    = error "Chyba!"
 in cr

dist p1 p2 = abs (x p2 - x p1) + abs (y p2 - y p1)

testing = do
    print $ cross (1, 0) (1, 10) (-1, 2) (3, 2)
    print $ cross (1, 0) (1, 10) (-1, 0) (3, 0)
    print $ cross (1, 0) (1, 10) (-1, 10) (3, 10)
    print $ cross (1, 0) (1, 10) (1, 0) (5, 0)
    print $ cross (1, 0) (1, 10) (2, 0) (5, 0)
    print $ cross (1, 0) (1, 10) (0, 0) (5, 0)
