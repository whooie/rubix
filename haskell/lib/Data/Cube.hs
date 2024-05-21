-- | Definitions for Rubik's cubes.
module Data.Cube
  ( Cube
  , new
  , isSolved
  , permute
  , movePerms
  , doMove
  , seqPerms
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

-- | Get the specific permutations denoted by a move.
movePerms :: Move -> [Perm]
movePerms F   = [ [ 0,  2,  8,  6]
                , [ 1,  5,  7,  3]
                , [29, 44, 15, 45]
                , [32, 43, 12, 46]
                , [35, 42,  9, 47] ]
movePerms F'  = [ [ 0,  6,  8,  2]
                , [ 1,  3,  7,  5]
                , [29, 45, 15, 44]
                , [32, 46, 12, 43]
                , [35, 47,  9, 42] ]
movePerms F2  = [ [ 0,  8], [ 2,  6]
                , [ 1,  7], [ 5,  3]
                , [29, 15], [44, 45]
                , [32, 12], [43, 46]
                , [35,  9], [42, 47] ]
movePerms B   = [ [18, 20, 26, 24]
                , [19, 23, 25, 21]
                , [11, 36, 33, 53]
                , [14, 37, 30, 52]
                , [17, 38, 27, 51] ]
movePerms B'  = [ [18, 24, 26, 20]
                , [19, 21, 25, 23]
                , [11, 53, 33, 36]
                , [14, 52, 30, 37]
                , [17, 51, 27, 38] ]
movePerms B2  = [ [18, 26], [20, 24]
                , [19, 25], [23, 21]
                , [11, 33], [36, 53]
                , [14, 30], [37, 52]
                , [17, 27], [38, 51] ]
movePerms L   = [ [27, 29, 35, 33]
                , [28, 32, 34, 30]
                , [20, 42,  6, 51]
                , [23, 39,  3, 48]
                , [26, 36,  0, 45] ]
movePerms L'  = [ [27, 33, 35, 29]
                , [28, 30, 34, 32]
                , [20, 51,  6, 42]
                , [23, 48,  3, 39]
                , [26, 45,  0, 36] ]
movePerms L2  = [ [27, 35], [29, 33]
                , [28, 34], [32, 30]
                , [20,  6], [42, 51]
                , [23,  3], [39, 48]
                , [26,  0], [36, 45] ]
movePerms R   = [ [ 9, 11, 17, 15]
                , [10, 14, 16, 12]
                , [ 2, 38, 24, 47]
                , [ 5, 41, 21, 50]
                , [ 8, 44, 18, 53] ]
movePerms R'  = [ [ 9, 15, 17, 11]
                , [10, 12, 16, 14]
                , [ 2, 47, 24, 38]
                , [ 5, 50, 21, 41]
                , [ 8, 53, 18, 44] ]
movePerms R2  = [ [ 9, 17], [11, 15]
                , [10, 16], [14, 12]
                , [ 2, 24], [38, 47]
                , [ 5, 21], [41, 50]
                , [ 8, 18], [44, 53] ]
movePerms U   = [ [36, 38, 44, 42]
                , [37, 41, 43, 39]
                , [27, 18,  9,  0]
                , [28, 19, 10,  1]
                , [29, 20, 11,  2] ]
movePerms U'  = [ [36, 42, 44, 38]
                , [37, 39, 43, 41]
                , [27,  0,  9, 18]
                , [28,  1, 10, 19]
                , [29,  2, 11, 20] ]
movePerms U2  = [ [36, 44], [38, 42]
                , [37, 43], [41, 39]
                , [27,  9], [18,  0]
                , [28, 10], [19,  1]
                , [29, 11], [20,  2] ]
movePerms D   = [ [45, 47, 53, 51]
                , [46, 50, 52, 48]
                , [35,  8, 17, 26]
                , [34,  7, 16, 25]
                , [33,  6, 15, 24] ]
movePerms D'  = [ [45, 51, 53, 47]
                , [46, 48, 52, 50]
                , [35, 26, 17,  8]
                , [34, 25, 16,  7]
                , [33, 24, 15,  6] ]
movePerms D2  = [ [45, 53], [47, 51]
                , [46, 52], [50, 48]
                , [35, 17], [ 8, 26]
                , [34, 16], [ 7, 25]
                , [33, 15], [ 6, 24] ]
movePerms FF  = movePerms F  ++ movePerms S
movePerms FF' = movePerms F' ++ movePerms S'
movePerms FF2 = movePerms F2 ++ movePerms S2
movePerms BB  = movePerms B  ++ movePerms S'
movePerms BB' = movePerms B' ++ movePerms S
movePerms BB2 = movePerms B2 ++ movePerms S2
movePerms LL  = movePerms L  ++ movePerms M
movePerms LL' = movePerms L' ++ movePerms M'
movePerms LL2 = movePerms L2 ++ movePerms M2
movePerms RR  = movePerms R  ++ movePerms M'
movePerms RR' = movePerms R' ++ movePerms M
movePerms RR2 = movePerms R2 ++ movePerms M2
movePerms UU  = movePerms U  ++ movePerms E'
movePerms UU' = movePerms U' ++ movePerms E
movePerms UU2 = movePerms U2 ++ movePerms E2
movePerms DD  = movePerms D  ++ movePerms E
movePerms DD' = movePerms D' ++ movePerms E'
movePerms DD2 = movePerms D2 ++ movePerms E2
movePerms M   = [ [ 1, 46, 25, 37]
                , [ 4, 49, 22, 40]
                , [ 7, 52, 19, 43] ]
movePerms M'  = [ [ 1, 37, 25, 46]
                , [ 4, 40, 22, 49]
                , [ 7, 43, 19, 52] ]
movePerms M2  = [ [ 1, 25], [46, 37]
                , [ 4, 22], [49, 40]
                , [ 7, 19], [43, 52] ]
movePerms E   = [ [ 3, 12, 21, 30]
                , [ 4, 13, 22, 31]
                , [ 5, 14, 23, 32] ]
movePerms E'  = [ [ 3, 30, 21, 12]
                , [ 4, 31, 22, 13]
                , [ 5, 32, 23, 14] ]
movePerms E2  = [ [ 3, 21], [30, 12]
                , [ 4, 22], [31, 13]
                , [ 5, 23], [32, 14] ]
movePerms S   = [ [39, 10, 50, 34]
                , [40, 13, 49, 31]
                , [41, 16, 48, 28] ]
movePerms S'  = [ [39, 34, 50, 10]
                , [40, 31, 49, 13]
                , [41, 28, 48, 16] ]
movePerms S2  = [ [39, 50], [34, 10]
                , [40, 49], [31, 13]
                , [41, 48], [28, 16] ]
movePerms X   = movePerms R' ++ movePerms M  ++ movePerms L
movePerms X'  = movePerms R  ++ movePerms M' ++ movePerms L'
movePerms X2  = movePerms R2 ++ movePerms M2 ++ movePerms L2
movePerms Y   = movePerms U  ++ movePerms E' ++ movePerms D'
movePerms Y'  = movePerms U' ++ movePerms E  ++ movePerms D
movePerms Y2  = movePerms U2 ++ movePerms E2 ++ movePerms D2
movePerms Z   = movePerms F  ++ movePerms S  ++ movePerms B'
movePerms Z'  = movePerms F' ++ movePerms S' ++ movePerms B
movePerms Z2  = movePerms F2 ++ movePerms S2 ++ movePerms B2

-- | Perform a move.
doMove :: Move -> Cube -> Cube
doMove move cube = foldl (flip permute) cube $ movePerms move

-- | Get the specific permutations denoted by a sequence.
seqPerms :: Seq -> [Perm]
seqPerms = concatMap movePerms

-- | Perform a sequence.
doSeq :: Seq -> Cube -> Cube
doSeq seq cube = foldl (flip doMove) cube seq

