{-# LANGUAGE DerivingVia #-}
module Logic where
import Prelude hiding (repeat, Right, Left)
import Data.Group
import Control.Monad (mfilter)
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid (Endo)
import Data.Semigroup (appEndo)

data Position = P Int Int deriving (Eq,Show)

initial :: Int -> Int -> Position
initial = P

data Direction = Left | Right | Up | Down deriving (Eq, Show, Enum, Bounded)


data Move = Stay |
            Step Direction |
            Combine Move Move deriving (Eq,Show)

newtype Move2 = Move2  { unMove2 :: Endo Position } deriving (Semigroup, Monoid) via (Endo Position)

runMove2 :: Move2 -> Position -> Position
runMove2 = appEndo . unMove2

dontMove :: Move
dontMove = Stay

step :: Direction -> Move
step = Step

left :: Move
left = step Left

right :: Move
right = step Right

up :: Move
up = step Up

down :: Move
down = step Down

rightDown :: Move
rightDown = right <> down

twoRightOneDown :: Move
twoRightOneDown = right <> right <> down

shift :: (Int, Int) -> Position -> Position
shift (u, v) (P x y) = P (x + u) (y + v)

directionToPair :: Direction -> (Int, Int)
directionToPair Left = (-1, 0)
directionToPair Right = (1, 0)
directionToPair Up = (0, 1)
directionToPair Down = (0, -1)

runMove :: Move -> Position -> Position
runMove Stay = id
runMove (Step d) = shift (directionToPair d)
runMove (Combine m1 m2) = runMove m2 . runMove m1

instance Semigroup Move where
  m1 <> m2 = Combine m1 m2

instance Monoid Move where
  mempty = Stay

instance Group Move where
  invert Stay = Stay
  invert (Step Right) = Step Left
  invert (Step Left) = Step Right
  invert (Step Up) = Step Down
  invert (Step Down) = Step Up
  invert (Combine m1 m2) = Combine (invert m2) (invert m1)


simplify :: Move -> Move
simplify Stay = Stay
simplify s@(Step _) = s
simplify (Combine m1 m2) = case (simplify m1, simplify m2) of
     (Stay, p) -> p
     (p , Stay) -> p
     (Step Left, Step Right) -> Stay
     (Step Right, Step Left) -> Stay
     (p1, p2) -> Combine p1 p2

data Range = Range Int Int deriving (Eq,Show)

inside :: Range -> Int -> Bool
inside (Range x y) t = x <= t && t < y

data Board = RectangularBoard {
   xRange :: Range, yRange :: Range } deriving (Eq,Show)

squareBoard :: Int -> Board
squareBoard = RectangularBoard <$> range <*> range where
   range = Range 0

move :: Board -> Move -> Position -> Maybe Position
move (RectangularBoard w h) m = mfilter checkInside . pure . runMove m where
   checkInside (P x y) = inside w x && inside h y

data Strategy = Moves (NonEmpty Move)
        deriving (Eq, Show)

oneStep :: Move -> Strategy
oneStep = Moves . N.singleton

andThen :: Strategy -> Strategy -> Strategy
andThen (Moves m1) (Moves m2) = Moves ( m2 <> m1 ) -- think what order is more readable

repeat :: Strategy -> Strategy
repeat = undefined

fill :: Move -> Strategy
fill = repeat . oneStep

horizontal :: Strategy
horizontal = repeat $ fill right
       `andThen` oneStep down
       `andThen` fill left
       `andThen` oneStep down

vertical :: Strategy
vertical  = repeat $ fill down
       `andThen` oneStep right
       `andThen` fill up
       `andThen` oneStep right


run :: Strategy -> Board -> Position -> [Position]
run (Moves ms) b p0  = snd $ foldr f (pure p0, []) ms where
     f m (Just p, ps) = case move b m p of
              Just p' -> (Just p' , ps ++ [p'])
              Nothing -> (Nothing, ps)
     f _ (Nothing, ps) = (Nothing, ps)



