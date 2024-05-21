-- | Rubik's cube group abstractions.
module Group
  ( Perm
  , Move (..)
  , moveInvert
  , Seq
  , seqInvert
  , commutator
  , (#@)
  , conjugate
  , (#:)
  , compose
  , (#.)
  ) where

-- | Type alias for a permutation specification.
type Perm = [Int]

-- | A move to perform on the cube.
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

-- | Get the inverse of a move.
moveInvert :: Move -> Move
moveInvert F   = F'
moveInvert F'  = F
moveInvert F2  = F2
moveInvert B   = B'
moveInvert B'  = B
moveInvert B2  = B2
moveInvert L   = L'
moveInvert L'  = L
moveInvert L2  = L2
moveInvert R   = R'
moveInvert R'  = R
moveInvert R2  = R2
moveInvert U   = U'
moveInvert U'  = U
moveInvert U2  = U2
moveInvert D   = D'
moveInvert D'  = D
moveInvert D2  = D2
moveInvert FF  = FF'
moveInvert FF' = FF
moveInvert FF2 = FF2
moveInvert BB  = BB'
moveInvert BB' = BB
moveInvert BB2 = BB2
moveInvert LL  = LL'
moveInvert LL' = LL
moveInvert LL2 = LL2
moveInvert RR  = RR'
moveInvert RR' = RR
moveInvert RR2 = RR2
moveInvert UU  = UU'
moveInvert UU' = UU
moveInvert UU2 = UU2
moveInvert DD  = DD'
moveInvert DD' = DD
moveInvert DD2 = DD2
moveInvert M   = M'
moveInvert M'  = M
moveInvert M2  = M2
moveInvert E   = E'
moveInvert E'  = E
moveInvert E2  = E2
moveInvert S   = S'
moveInvert S'  = S
moveInvert S2  = S2
moveInvert X   = X'
moveInvert X'  = X
moveInvert X2  = X2
moveInvert Y   = Y'
moveInvert Y'  = Y
moveInvert Y2  = Y2
moveInvert Z   = Z'
moveInvert Z'  = Z
moveInvert Z2  = Z2

-- | Return @True@ if two moves are inverses.
moveIsInverse :: Move -> Move -> Bool
moveIsInverse a b = a == moveInvert b

-- | Return @True@ if two moves commute.
moveCommutes :: Move -> Move -> Bool
moveCommutes F   B   = True
moveCommutes F   B'  = True
moveCommutes F   B2  = True
moveCommutes F'  B   = True
moveCommutes F'  B'  = True
moveCommutes F'  B2  = True
moveCommutes F2  B   = True
moveCommutes F2  B'  = True
moveCommutes F2  B2  = True
moveCommutes F   S   = True
moveCommutes F   S'  = True
moveCommutes F   S2  = True
moveCommutes F'  S   = True
moveCommutes F'  S'  = True
moveCommutes F'  S2  = True
moveCommutes F2  S   = True
moveCommutes F2  S'  = True
moveCommutes F2  S2  = True
moveCommutes F   BB  = True
moveCommutes F   BB' = True
moveCommutes F   BB2 = True
moveCommutes F'  BB  = True
moveCommutes F'  BB' = True
moveCommutes F'  BB2 = True
moveCommutes F2  BB  = True
moveCommutes F2  BB' = True
moveCommutes F2  BB2 = True
moveCommutes FF  B   = True
moveCommutes FF  B'  = True
moveCommutes FF  B2  = True
moveCommutes FF' B   = True
moveCommutes FF' B'  = True
moveCommutes FF' B2  = True
moveCommutes FF2 B   = True
moveCommutes FF2 B'  = True
moveCommutes FF2 B2  = True
moveCommutes B   F   = True
moveCommutes B   F'  = True
moveCommutes B   F2  = True
moveCommutes B'  F   = True
moveCommutes B'  F'  = True
moveCommutes B'  F2  = True
moveCommutes B2  F   = True
moveCommutes B2  F'  = True
moveCommutes B2  F2  = True
moveCommutes B   S   = True
moveCommutes B   S'  = True
moveCommutes B   S2  = True
moveCommutes B'  S   = True
moveCommutes B'  S'  = True
moveCommutes B'  S2  = True
moveCommutes B2  S   = True
moveCommutes B2  S'  = True
moveCommutes B2  S2  = True
moveCommutes B   FF  = True
moveCommutes B   FF' = True
moveCommutes B   FF2 = True
moveCommutes B'  FF  = True
moveCommutes B'  FF' = True
moveCommutes B'  FF2 = True
moveCommutes B2  FF  = True
moveCommutes B2  FF' = True
moveCommutes B2  FF2 = True
moveCommutes BB  F   = True
moveCommutes BB  F'  = True
moveCommutes BB  F2  = True
moveCommutes BB' F   = True
moveCommutes BB' F'  = True
moveCommutes BB' F2  = True
moveCommutes BB2 F   = True
moveCommutes BB2 F'  = True
moveCommutes BB2 F2  = True
moveCommutes L   R   = True
moveCommutes L   R'  = True
moveCommutes L   R2  = True
moveCommutes L'  R   = True
moveCommutes L'  R'  = True
moveCommutes L'  R2  = True
moveCommutes L2  R   = True
moveCommutes L2  R'  = True
moveCommutes L2  R2  = True
moveCommutes L   M   = True
moveCommutes L   M'  = True
moveCommutes L   M2  = True
moveCommutes L'  M   = True
moveCommutes L'  M'  = True
moveCommutes L'  M2  = True
moveCommutes L2  M   = True
moveCommutes L2  M'  = True
moveCommutes L2  M2  = True
moveCommutes L   RR  = True
moveCommutes L   RR' = True
moveCommutes L   RR2 = True
moveCommutes L'  RR  = True
moveCommutes L'  RR' = True
moveCommutes L'  RR2 = True
moveCommutes L2  RR  = True
moveCommutes L2  RR' = True
moveCommutes L2  RR2 = True
moveCommutes LL  R   = True
moveCommutes LL  R'  = True
moveCommutes LL  R2  = True
moveCommutes LL' R   = True
moveCommutes LL' R'  = True
moveCommutes LL' R2  = True
moveCommutes LL2 R   = True
moveCommutes LL2 R'  = True
moveCommutes LL2 R2  = True
moveCommutes R   L   = True
moveCommutes R   L'  = True
moveCommutes R   L2  = True
moveCommutes R'  L   = True
moveCommutes R'  L'  = True
moveCommutes R'  L2  = True
moveCommutes R2  L   = True
moveCommutes R2  L'  = True
moveCommutes R2  L2  = True
moveCommutes R   M   = True
moveCommutes R   M'  = True
moveCommutes R   M2  = True
moveCommutes R'  M   = True
moveCommutes R'  M'  = True
moveCommutes R'  M2  = True
moveCommutes R2  M   = True
moveCommutes R2  M'  = True
moveCommutes R2  M2  = True
moveCommutes R   LL  = True
moveCommutes R   LL' = True
moveCommutes R   LL2 = True
moveCommutes R'  LL  = True
moveCommutes R'  LL' = True
moveCommutes R'  LL2 = True
moveCommutes R2  LL  = True
moveCommutes R2  LL' = True
moveCommutes R2  LL2 = True
moveCommutes RR  L   = True
moveCommutes RR  L'  = True
moveCommutes RR  L2  = True
moveCommutes RR' L   = True
moveCommutes RR' L'  = True
moveCommutes RR' L2  = True
moveCommutes RR2 L   = True
moveCommutes RR2 L'  = True
moveCommutes RR2 L2  = True
moveCommutes U   D   = True
moveCommutes U   D'  = True
moveCommutes U   D2  = True
moveCommutes U'  D   = True
moveCommutes U'  D'  = True
moveCommutes U'  D2  = True
moveCommutes U2  D   = True
moveCommutes U2  D'  = True
moveCommutes U2  D2  = True
moveCommutes U   E   = True
moveCommutes U   E'  = True
moveCommutes U   E2  = True
moveCommutes U'  E   = True
moveCommutes U'  E'  = True
moveCommutes U'  E2  = True
moveCommutes U2  E   = True
moveCommutes U2  E'  = True
moveCommutes U2  E2  = True
moveCommutes U   DD  = True
moveCommutes U   DD' = True
moveCommutes U   DD2 = True
moveCommutes U'  DD  = True
moveCommutes U'  DD' = True
moveCommutes U'  DD2 = True
moveCommutes U2  DD  = True
moveCommutes U2  DD' = True
moveCommutes U2  DD2 = True
moveCommutes UU  D   = True
moveCommutes UU  D'  = True
moveCommutes UU  D2  = True
moveCommutes UU' D   = True
moveCommutes UU' D'  = True
moveCommutes UU' D2  = True
moveCommutes UU2 D   = True
moveCommutes UU2 D'  = True
moveCommutes UU2 D2  = True
moveCommutes D   U   = True
moveCommutes D   U'  = True
moveCommutes D   U2  = True
moveCommutes D'  U   = True
moveCommutes D'  U'  = True
moveCommutes D'  U2  = True
moveCommutes D2  U   = True
moveCommutes D2  U'  = True
moveCommutes D2  U2  = True
moveCommutes D   E   = True
moveCommutes D   E'  = True
moveCommutes D   E2  = True
moveCommutes D'  E   = True
moveCommutes D'  E'  = True
moveCommutes D'  E2  = True
moveCommutes D2  E   = True
moveCommutes D2  E'  = True
moveCommutes D2  E2  = True
moveCommutes D   UU  = True
moveCommutes D   UU' = True
moveCommutes D   UU2 = True
moveCommutes D'  UU  = True
moveCommutes D'  UU' = True
moveCommutes D'  UU2 = True
moveCommutes D2  UU  = True
moveCommutes D2  UU' = True
moveCommutes D2  UU2 = True
moveCommutes DD  U   = True
moveCommutes DD  U'  = True
moveCommutes DD  U2  = True
moveCommutes DD' U   = True
moveCommutes DD' U'  = True
moveCommutes DD' U2  = True
moveCommutes DD2 U   = True
moveCommutes DD2 U'  = True
moveCommutes DD2 U2  = True
moveCommutes a b = moveIsInverse a b

-- | A sequence of moves.
type Seq = [Move]

-- | Get the inverse of a sequence.
seqInvert :: Seq -> Seq
seqInvert = map moveInvert . reverse

-- | Compose two sequences, making simplifications where possible.
compose :: Seq -> Seq -> Seq
compose = error "todo"

-- | Infix operator for `compose`.
(#.) :: Seq -> Seq -> Seq
(#.) = compose
infixl 7 #.

-- | Get the commutator of two sequences, @[A, B] = A B A' B'@.
commutator :: Seq -> Seq -> Seq
commutator a b = a #. b #. seqInvert a #. seqInvert b

-- | Infix operator for `commutator`.
(#@) :: Seq -> Seq -> Seq
(#@) = commutator
infix 6 #@

-- | Get the conjugate of two sequences, @[A : B] = A B A'@.
conjugate :: Seq -> Seq -> Seq
conjugate a b = a #. b #. seqInvert a

-- | Infix operator for `conjugate`.
(#:) :: Seq -> Seq -> Seq
(#:) = conjugate
infix 6 #:

