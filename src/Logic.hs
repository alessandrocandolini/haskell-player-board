{-# LANGUAGE LambdaCase #-}

module Logic where

import Control.Monad (mfilter)
import Control.Monad.State
import Data.Group
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Maybe (maybeToList)
import Prelude hiding (Left, Right, repeat)

data Position = P Int Int deriving (Eq, Show)

initial :: Int -> Int -> Position
initial = P

data Direction = Left | Right | Up | Down deriving (Eq, Show, Enum, Bounded)

data Move
  = Stay
  | Step Direction
  | Combine Move Move
  deriving (Eq, Show)

dontMove :: Move
dontMove = Stay

left :: Move
left = Step Left

right :: Move
right = Step Right

up :: Move
up = Step Up

down :: Move
down = Step Down

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
  (p, Stay) -> p
  (Step Left, Step Right) -> Stay
  (Step Right, Step Left) -> Stay
  (p1, p2) -> Combine p1 p2

data Range = Range Int Int deriving (Eq, Show)

inside :: Range -> Int -> Bool
inside (Range x y) t = x <= t && t < y

data Board
  = InfiniteBoard
  | RectangularBoard
      { xRange :: Range,
        yRange :: Range
      }
  deriving (Eq, Show)

squareBoard :: Int -> Board
squareBoard n = rectangularBoard n n

rectangularBoard :: Int -> Int -> Board
rectangularBoard w h = RectangularBoard (range w) (range h)
  where
    range = Range 0

move :: Board -> Move -> Position -> Maybe Position
move (RectangularBoard w h) m = mfilter checkInside . pure . runMove m
  where
    checkInside (P x y) = inside w x && inside h y
move InfiniteBoard m = pure . runMove m

data Strategy
  = OneStep Move
  | AndThen Strategy Strategy
  | OrElse Strategy Strategy
  | Repeat Strategy
  deriving (Eq, Show)

oneStep :: Move -> Strategy
oneStep = OneStep

andThen :: Strategy -> Strategy -> Strategy
andThen = AndThen

orElse :: Strategy -> Strategy -> Strategy
orElse = OrElse

repeat :: Strategy -> Strategy
repeat = Repeat

fill :: Move -> Strategy
fill = repeat . oneStep

horizontal :: Strategy
horizontal =
  repeat $
    fill right
      `andThen` oneStep up
      `andThen` fill left
      `andThen` oneStep up

vertical :: Strategy
vertical =
  repeat $
    fill up
      `andThen` oneStep right
      `andThen` fill down
      `andThen` oneStep right

diagonal :: Strategy
diagonal = repeat $
   oneStep right `andThen` oneStep up

run' :: Strategy -> Board -> State (Maybe Position, [Position]) ()
run' (OneStep m) b = do
  (p, ps) <- get
  let updated = p >>= move b m
  put (updated, maybeToList updated ++ ps)
run' (AndThen s1 s2) b =
  run' s1 b >> run' s2 b
run' (OrElse s1 s2) b = do
  (p, ps) <- get
  _ <- run' s1 b
  (p', _) <- get
  case p' of
    Just _ -> pure ()
    Nothing -> do
      _ <- put (p, ps)
      _ <- run' s2 b
      return ()
run' (Repeat s) b = do
  p <- get
  _ <- run' s b
  (p', _) <- get
  case p' of
    Just _ -> run' (Repeat s) b
    Nothing -> do
       put p
       pure ()

run :: Strategy -> Board -> Position -> [Position]
run s b p = snd $ execState (run' s b) (pure p, [])
