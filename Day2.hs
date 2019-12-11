import Prelude hiding (read, length, head)
import qualified Prelude as P (read)
import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed (Vector, fromList, thaw, freeze, head, unsafeFreeze, unsafeThaw)
import Data.Vector.Unboxed.Mutable

main = part2

part1 = loadData "day2-input" >>= print . head . process . vinit 12 2 . fromList

part2 = let
    inputs = [ (noun, verb) | noun <- [0..99], verb <- [0..99] ]
  in do
    dat <- loadData "day2-input" >>= return . fromList
    loop inputs dat
    where
      result = 19690720
      loop ((noun,verb):is) d = if (head . process . vinit noun verb $ d) == result
        then do
          print $ (show noun) ++ ", " ++ (show verb)
          print $ 100 * noun + verb
        else
          loop is d


mtail (x:xs) = xs
mtail _ = []

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn spl str = let splitted = span (\c -> c /= spl) str
                  in  fst splitted : splitOn spl (mtail $ snd splitted)

loadData filename = readFile filename >>= return . map P.read . splitOn ',' :: IO [Int]

vinit noun verb dat = runST $ do
  mut <- thaw dat
  write mut 1 noun
  write mut 2 verb
  unsafeFreeze mut

process :: Vector Int -> Vector Int
process dat = runST $ do
  mut <- unsafeThaw dat
  --init mut
  doLogic 0 mut
  unsafeFreeze mut
  where
    doLogic pos dat = do
      if pos <= length dat then do
        opcode <- read dat pos
        case opcode of
          1  -> performOp (+) pos dat >> doLogic (pos+4) dat
          2  -> performOp (*) pos dat >> doLogic (pos+4) dat
          99 -> return ()
      else return ()
    performOp op pos dat = do
      idx1 <- read dat (pos+1)
      val1 <- read dat (idx1)
      idx2 <- read dat (pos+2)
      val2 <- read dat (idx2)
      idxr <- read dat (pos+3)
      write dat idxr (op val1 val2)
