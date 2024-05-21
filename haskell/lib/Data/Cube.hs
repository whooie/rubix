-- | Definitions for Rubik's cubes.
module Data.Cube
  ( Cube
  , new
  , isSolved
  , permute
  , doMove
  , doSeq
  ) where

import Data.List (uncons)
import Group

-- | A Rubik's cube.
--
-- Sticker positions are indexed as below:
--
-- >                        20   19   18
-- >                      .--------------.
-- >                   27 | 36 | 37 | 38 | 11
-- >                      |----+----+----|
-- >                   28 | 39 | 40 | 41 | 10
-- >                      |----+----+----|
-- >                   29 | 42 | 43 | 44 |  9
-- >                      '--------------'
-- >      36   39   42           ||           44   41   38      38   37   36
-- >    .--------------.  .--------------.  .--------------.  .--------------.
-- > 20 | 27 | 28 | 29 |  |  0 |  1 |  2 |  |  9 | 10 | 11 |  | 18 | 19 | 20 | 27
-- >    |----+----+----|  |----+----+----|  |----+----+----|  |----+----+----|
-- > 23 | 30 | 31 | 32 |==|  3 |  4 |  5 |==| 12 | 13 | 14 |==| 21 | 22 | 23 | 30
-- >    |----+----+----|  |----+----+----|  |----+----+----|  |----+----+----|
-- > 26 | 33 | 34 | 35 |  |  6 |  7 |  8 |  | 15 | 16 | 17 |  | 24 | 25 | 26 | 33
-- >    '--------------'  '--------------'  '--------------'  '--------------'
-- >      51   48   45           ||           47   50   53      53   52   51
-- >                      .--------------.
-- >                   35 | 45 | 46 | 47 | 15
-- >                      |----+----+----|
-- >                   34 | 48 | 49 | 50 | 16
-- >                      |----+----+----|
-- >                   33 | 51 | 52 | 53 | 17
-- >                      '--------------'
-- >                        26   25   24
data Cube = Cube [Int] deriving (Eq)

-- | Create a new, solved Rubik's cube.    
new :: Cube
new = Cube [0..53]

-- | Return @True@ if the cube is solved.
isSolved :: Cube -> Bool
isSolved = (new ==) -- todo: check for overall rotations

-- | Perform a cyclic permutation of any series of spots on the cube (see map
-- above). The permutation @[a_1, ..., a_n]@ takes the sticker at @a_k@ to @a_(k
-- + 1)@, and @a_n@ to @a_1@. Does nothing if any element of the permutation is
-- outside the range @[0..53]@.
permute :: Perm -> Cube -> Cube
permute idx (Cube cc) = Cube $ permuteList idx cc

permuteList :: Perm -> [a] -> [a]
permuteList _ [] = []
permuteList [] items = items
permuteList (_ : []) items = items
permuteList idx items =
  let n = length items
   in if all (\k -> 0 <= k && k < n) idx
      then permuteListInner idx items
      else items

unwrap :: Maybe a -> a
unwrap (Just it) = it
unwrap Nothing   = error "unreachable"

permuteListInner :: Perm -> [a] -> [a]
permuteListInner idx items =
  let (first, _) = unwrap $ uncons idx
      wrapped = Nothing : map Just items
      permed = foldl (\acc k -> swapList 0 (k + 1) acc) wrapped idx
      fixed = swapList 0 (first + 1) permed
   in map unwrap $ tail fixed

swapList :: Int -> Int -> [a] -> [a]
swapList a b items = swapListLeft (min a b) (max a b) items
  where swapListLeft :: Int -> Int -> [a] -> [a]
        swapListLeft _ _ [] = []
        swapListLeft 0 b (it : tail) =
          let (tail', maybeItB) = swapListRight it (b - 1) tail
           in case maybeItB of
                Just itB -> itB : tail'
                Nothing -> it : tail'
        swapListLeft a b (it : tail) = it : tail'
          where tail' = swapListLeft (a - 1) (b - 1) tail
        swapListRight :: a -> Int -> [a] -> ([a], Maybe a)
        swapListRight _ _ [] = ([], Nothing)
        swapListRight itA 0 (it : tail) = (itA : tail, Just it)
        swapListRight itA b (it : tail) = (it : tail', maybeItB)
          where (tail', maybeItB) = swapListRight itA (b - 1) tail

-- | Perform a move.
doMove :: RubiksGen g => g -> Cube -> Cube
doMove move cube = foldl (flip permute) cube $ perms $ move

-- | Perform a sequence.
doSeq :: RubiksGen g => [g] -> Cube -> Cube
doSeq seq cube = foldl (flip doMove) cube seq

