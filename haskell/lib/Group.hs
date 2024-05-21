-- | Rubik's cube group abstractions.
module Group
  ( Perm
  , Move (..)
  , Move' (..)
  , Axis (..)
  , Slice (..)
  , Rot (..)
  , (@+)
  , (@-)
  , rInv
  , RubiksGen (..)
  , seqPerms
  , seqInverse
  , compose
  , (#.)
  , commutator
  , (#~)
  , conjugate
  , (#:)
  ) where

import Data.Kind (Type)

-- | Type alias for a permutation specification.
type Perm = [Int]

-- | Basic description of a move to perform on the cube.
--
-- See also `Move'` and `toMove'`.
data Move = F  | F'  | F2
          | B  | B'  | B2
          | L  | L'  | L2
          | R  | R'  | R2
          | U  | U'  | U2
          | D  | D'  | D2
          | FF | FF' | FF2
          | BB | BB' | BB2
          | LL | LL' | LL2
          | RR | RR' | RR2
          | UU | UU' | UU2
          | DD | DD' | DD2
          | M  | M'  | M2
          | E  | E'  | E2
          | S  | S'  | S2
          | X  | X'  | X2
          | Y  | Y'  | Y2
          | Z  | Z'  | Z2
          | Id
          deriving (Eq, Enum)

instance Show Move where
  show F   = "F"
  show F'  = "F'"
  show F2  = "F2"
  show B   = "B"
  show B'  = "B'"
  show B2  = "B2"
  show L   = "L"
  show L'  = "L'"
  show L2  = "L2"
  show R   = "R"
  show R'  = "R'"
  show R2  = "R2"
  show U   = "U"
  show U'  = "U'"
  show U2  = "U2"
  show D   = "D"
  show D'  = "D'"
  show D2  = "D2"
  show FF  = "f"
  show FF' = "f'"
  show FF2 = "f2"
  show BB  = "b"
  show BB' = "b'"
  show BB2 = "b2"
  show LL  = "l"
  show LL' = "l'"
  show LL2 = "l2"
  show RR  = "r"
  show RR' = "r'"
  show RR2 = "r2"
  show UU  = "u"
  show UU' = "u'"
  show UU2 = "u2"
  show DD  = "d"
  show DD' = "d'"
  show DD2 = "d2"
  show M   = "M"
  show M'  = "M'"
  show M2  = "M2"
  show E   = "E"
  show E'  = "E'"
  show E2  = "E2"
  show S   = "S"
  show S'  = "S'"
  show S2  = "S2"
  show X   = "x"
  show X'  = "x'"
  show X2  = "x2"
  show Y   = "y"
  show Y'  = "y'"
  show Y2  = "y2"
  show Z   = "z"
  show Z'  = "z'"
  show Z2  = "z2"
  show Id  = "I"

-- | Analytic description of a `Move`, where the move axis, slice, and rotation
-- of the move are described separately.
data Move' = Move' Axis Slice Rot deriving (Eq, Show)

-- | An axis of the cube.
--
-- >              .--------------.
-- >             /    /    /    /|
-- >            /--------------/ |
-- >           /    /    /    /|/|
-- >          /--------------/ | |
-- >         /    /    /    /|/|/|
-- >        .--------------. | | '  AZ
-- >     |  |    |    |    |/|/|/  /
-- >     |  |----+----+----| | /  /
-- >     |  |    |    |    |/|/  /
-- >     |  |----+----+----| /  /
-- >     |  |    |    |    |/  /
-- >     V  '--------------'
-- >    AY   --------------> AX
--
-- @AI@ is an identity/null element used for conversion between `Move` and
-- `Move'`.
data Axis = AX | AY | AZ | AI deriving (Eq, Enum, Show)


-- | A slice of the cube on any axis.
--
-- >              .--------------.
-- >             /    /    /    /|
-- >            /--------------/ |
-- >           /    /    /    /|/|
-- >          /--------------/ | |
-- >         /    /    /    /|/|/|
-- >        .--------------. | | '
-- >   P0 > |    |    |    |/|/|/ < P2
-- >        |----+----+----| | /
-- >   P1 > |    |    |    |/|/ < P1
-- >        |----+----+----| /
-- >   P2 > |    |    |    |/ < P0
-- >        '--------------'
-- >          ^P0  ^P1  ^P2
-- >
-- > S01  = S0 + S1
-- > S12  = S1 + S2
-- > S012 = S0 + S1 + S2
--
-- @SI@ is an identity/null element used for conversion between `Move` and
-- `Move'`.
data Slice = P0 | P1 | P2 | S01 | S12 | S012 | SI deriving (Eq, Enum, Show)

-- | A rotation about a fixed axis, defined with a right-hand rule about an
-- accompanying `Axis` in a `Move'`.
data Rot = I
         -- ^ No rotation
         | Q
         -- ^ One-quarter rotation
         | Q2
         -- ^ One-half rotation
         | Q'
         -- ^ Three-quarters rotation
         deriving (Eq, Show)

instance Enum Rot where
  fromEnum r = case r of
                 I  -> 0
                 Q  -> 1
                 Q2 -> 2
                 Q' -> 3
  toEnum m = case m `mod` 4 of
               0 -> I
               1 -> Q
               2 -> Q2
               3 -> Q'
               _ -> error "unreachable"

-- | Add a `Rot` to another.
(@+) :: Rot -> Rot -> Rot
(@+) r1 r2 = toEnum $ (fromEnum r1) + (fromEnum r2)
infixr 6 @+

-- | Subtract a `Rot` from another.
(@-) :: Rot -> Rot -> Rot
(@-) r1 r2 = toEnum $ (fromEnum r1) - (fromEnum r2)
infix 6 @-

-- | Invert a `Rot`.
rInv :: Rot -> Rot
rInv r = toEnum $ -(fromEnum r)

-- | Convert a `Move` to a `Move'`.
moveToMove' :: Move -> Move'
moveToMove' F   = Move' AZ P0   Q
moveToMove' F'  = Move' AZ P0   Q'
moveToMove' F2  = Move' AZ P0   Q2
moveToMove' B   = Move' AZ P2   Q'
moveToMove' B'  = Move' AZ P2   Q
moveToMove' B2  = Move' AZ P2   Q2
moveToMove' L   = Move' AX P0   Q
moveToMove' L'  = Move' AX P0   Q'
moveToMove' L2  = Move' AX P0   Q2
moveToMove' R   = Move' AX P2   Q'
moveToMove' R'  = Move' AX P2   Q
moveToMove' R2  = Move' AX P2   Q2
moveToMove' U   = Move' AY P0   Q
moveToMove' U'  = Move' AY P0   Q'
moveToMove' U2  = Move' AY P0   Q2
moveToMove' D   = Move' AY P2   Q'
moveToMove' D'  = Move' AY P2   Q
moveToMove' D2  = Move' AY P2   Q2
moveToMove' FF  = Move' AZ S01  Q
moveToMove' FF' = Move' AZ S01  Q'
moveToMove' FF2 = Move' AZ S01  Q2
moveToMove' BB  = Move' AZ S12  Q'
moveToMove' BB' = Move' AZ S12  Q
moveToMove' BB2 = Move' AZ S12  Q2
moveToMove' LL  = Move' AX S01  Q
moveToMove' LL' = Move' AX S01  Q'
moveToMove' LL2 = Move' AX S01  Q2
moveToMove' RR  = Move' AX S12  Q'
moveToMove' RR' = Move' AX S12  Q
moveToMove' RR2 = Move' AX S12  Q2
moveToMove' UU  = Move' AY S01  Q
moveToMove' UU' = Move' AY S01  Q'
moveToMove' UU2 = Move' AY S01  Q2
moveToMove' DD  = Move' AY S12  Q'
moveToMove' DD' = Move' AY S12  Q
moveToMove' DD2 = Move' AY S12  Q2
moveToMove' M   = Move' AX P1   Q
moveToMove' M'  = Move' AX P1   Q'
moveToMove' M2  = Move' AX P1   Q2
moveToMove' E   = Move' AY P1   Q'
moveToMove' E'  = Move' AY P1   Q
moveToMove' E2  = Move' AY P1   Q2
moveToMove' S   = Move' AZ P1   Q
moveToMove' S'  = Move' AZ P1   Q'
moveToMove' S2  = Move' AZ P1   Q2
moveToMove' X   = Move' AX S012 Q
moveToMove' X'  = Move' AX S012 Q'
moveToMove' X2  = Move' AX S012 Q2
moveToMove' Y   = Move' AY S012 Q
moveToMove' Y'  = Move' AY S012 Q'
moveToMove' Y2  = Move' AY S012 Q2
moveToMove' Z   = Move' AZ S012 Q
moveToMove' Z'  = Move' AZ S012 Q'
moveToMove' Z2  = Move' AZ S012 Q2
moveToMove' Id  = Move' AI SI   I

-- | Convert a `Move'` to a `Move`.
moveFromMove' :: Move' -> Move
moveFromMove' (Move' AI _    _ ) = Id
moveFromMove' (Move' _  SI   _ ) = Id
moveFromMove' (Move' _  _    I ) = Id
moveFromMove' (Move' AZ P0   Q ) = F
moveFromMove' (Move' AZ P0   Q') = F'
moveFromMove' (Move' AZ P0   Q2) = F2
moveFromMove' (Move' AZ P2   Q') = B
moveFromMove' (Move' AZ P2   Q ) = B'
moveFromMove' (Move' AZ P2   Q2) = B2
moveFromMove' (Move' AX P0   Q ) = L
moveFromMove' (Move' AX P0   Q') = L'
moveFromMove' (Move' AX P0   Q2) = L2
moveFromMove' (Move' AX P2   Q') = R
moveFromMove' (Move' AX P2   Q ) = R'
moveFromMove' (Move' AX P2   Q2) = R2
moveFromMove' (Move' AY P0   Q ) = U
moveFromMove' (Move' AY P0   Q') = U'
moveFromMove' (Move' AY P0   Q2) = U2
moveFromMove' (Move' AY P2   Q') = D
moveFromMove' (Move' AY P2   Q ) = D'
moveFromMove' (Move' AY P2   Q2) = D2
moveFromMove' (Move' AZ S01  Q ) = FF
moveFromMove' (Move' AZ S01  Q') = FF'
moveFromMove' (Move' AZ S01  Q2) = FF2
moveFromMove' (Move' AZ S12  Q') = BB
moveFromMove' (Move' AZ S12  Q ) = BB'
moveFromMove' (Move' AZ S12  Q2) = BB2
moveFromMove' (Move' AX S01  Q ) = LL
moveFromMove' (Move' AX S01  Q') = LL'
moveFromMove' (Move' AX S01  Q2) = LL2
moveFromMove' (Move' AX S12  Q') = RR
moveFromMove' (Move' AX S12  Q ) = RR'
moveFromMove' (Move' AX S12  Q2) = RR2
moveFromMove' (Move' AY S01  Q ) = UU
moveFromMove' (Move' AY S01  Q') = UU'
moveFromMove' (Move' AY S01  Q2) = UU2
moveFromMove' (Move' AY S12  Q') = DD
moveFromMove' (Move' AY S12  Q ) = DD'
moveFromMove' (Move' AY S12  Q2) = DD2
moveFromMove' (Move' AX P1   Q ) = M
moveFromMove' (Move' AX P1   Q') = M'
moveFromMove' (Move' AX P1   Q2) = M2
moveFromMove' (Move' AY P1   Q') = E
moveFromMove' (Move' AY P1   Q ) = E'
moveFromMove' (Move' AY P1   Q2) = E2
moveFromMove' (Move' AZ P1   Q ) = S
moveFromMove' (Move' AZ P1   Q') = S'
moveFromMove' (Move' AZ P1   Q2) = S2
moveFromMove' (Move' AX S012 Q ) = X
moveFromMove' (Move' AX S012 Q') = X'
moveFromMove' (Move' AX S012 Q2) = X2
moveFromMove' (Move' AY S012 Q ) = Y
moveFromMove' (Move' AY S012 Q') = Y'
moveFromMove' (Move' AY S012 Q2) = Y2
moveFromMove' (Move' AZ S012 Q ) = Z
moveFromMove' (Move' AZ S012 Q') = Z'
moveFromMove' (Move' AZ S012 Q2) = Z2

-- | Get the axis of a move.
moveAxis :: Move -> Axis
moveAxis move = ax
  where Move' ax _ _ = toMove' move

-- | Get the slice of a move.
moveSlice :: Move -> Slice
moveSlice move = sl
  where Move' _ sl _ = toMove' move

-- | Get the rotation of a move.
moveRot :: Move -> Rot
moveRot move = r
  where Move' _ _ r = toMove' move

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
movePerms Id  = []

-- | Get the inverse of a move.
moveInverse :: Move -> Move
moveInverse move = moveFromMove' $ Move' ax sl (rInv r)
  where Move' ax sl r = moveToMove' move

-- | Return @True@ if two moves are inverses.
moveIsInverse :: Move -> Move -> Bool
moveIsInverse a b = a == moveInverse b

-- | Return @True@ if two moves commute.
moveCommutesWith :: Move -> Move -> Bool
moveCommutesWith a b = axA == axB
  where Move' axA _ _ = moveToMove' a
        Move' axB _ _ = moveToMove' b

-- | Class for Rubik's cube group generators, limited to the conventional set of
-- basic moves describable as a set of rotations in the same direction about a
-- single axis, plus the identity.
--
-- __Laws:__
--
-- * /Identity elements:/ Any element @g@ denoting the identity must satisfy:
--
--     * @toMove g == "Id"@
--     * @toMove' g == Move' "AI" "SI" "I"@
--     * @axis g == "AI"@
--     * @slice g == "SI"@
--     * @rot g == "I"@
--     * @perms g == []@
--     * @inv g == g@
--     * @commutes g g == True@
--
-- * /Inverse elements:/ Any element @a@ and @b@ denoting inverses of each other
-- must satisfy:
--
--     * @axis a == axis b@
--     * @slice a == slice b@
--     * @rot a == rot b \@+ "Q2"@
--     * @a == inv b@
--     * @isInv a b == True@
--     * @commutes a b == True@
--
-- * /Orientation of axes:/ See `Axis` and `Slice`.
--
class RubiksGen g where
  -- | Convert to the base identifier.
  toMove :: g -> Move
  -- | Convert from the base identifier.
  fromMove :: Move -> g
  -- | Convert to the analytic identifier.
  toMove' :: g -> Move'
  toMove' = moveToMove' . toMove
  -- | Convert from the analytic identifier.
  fromMove' :: Move' -> g
  fromMove' = fromMove . moveFromMove'
  -- | Get the axis on which the group element acts.
  axis :: g -> Axis
  axis = moveAxis . toMove
  -- | Get the cube slice on which the group element acts.
  slice :: g -> Slice
  slice = moveSlice . toMove
  -- | Get the rotation applied by the group element.
  rot :: g -> Rot
  rot = moveRot . toMove
  -- | Get the list of permutations denoted by the group element.
  perms :: g -> [Perm]
  perms = movePerms . toMove
  -- | Return the inverse of the group element.
  inv :: g -> g
  inv = fromMove . moveInverse . toMove
  -- | Return @True@ if two elements are inverses.
  isInv :: g -> g -> Bool
  isInv a b = moveIsInverse (toMove a) (toMove b)
  -- | Return @True@ if two elements commute.
  commutes :: g -> g -> Bool
  commutes a b = moveCommutesWith (toMove a) (toMove b)

instance RubiksGen Move where
  toMove = id
  fromMove = id

instance RubiksGen Move' where
  toMove = moveFromMove'
  fromMove = moveToMove'

-- | Get the specific permutations denoted by a sequence.
seqPerms :: RubiksGen g => [g] -> [Perm]
seqPerms = concatMap perms

-- | Get the inverse of a sequence.
seqInverse :: RubiksGen g => [g] -> [g]
seqInverse = map inv . reverse

-- | Compose two sequences, making simplifications where possible.
compose :: RubiksGen g => [g] -> [g] -> [g]
compose = error "todo"

-- | Infix operator for `compose`.
(#.) :: RubiksGen g => [g] -> [g] -> [g]
(#.) = compose
infixl 7 #.

-- | Get the commutator of two sequences, @[A, B] = A B A' B'@.
commutator :: RubiksGen g => [g] -> [g] -> [g]
commutator a b = a #. b #. seqInverse a #. seqInverse b

-- | Infix operator for `commutator`.
(#~) :: RubiksGen g => [g] -> [g] -> [g]
(#~) = commutator
infix 6 #~

-- | Get the conjugate of two sequences, @[A : B] = A B A'@.
conjugate :: RubiksGen g => [g] -> [g] -> [g]
conjugate a b = a #. b #. seqInverse a

-- | Infix operator for `conjugate`.
(#:) :: RubiksGen g => [g] -> [g] -> [g]
(#:) = conjugate
infix 6 #:

-- | An element of the Rubiks cube group.
data G g = Gen g | Seq [g]

-- | Class for Rubik's cube group elements.
--
-- __Laws:__
--
class RubiksGroup (m :: Type -> Type) where
  -- | Get the list of permutations denoted by the group element.
  getPerms :: RubiksGen g => m g -> [Perm]
  -- | Return the inverse of the group element.
  getInv :: RubiksGen g => m g -> m g

