{-# LANGUAGE DerivingVia #-}
module Extras where

import Logic hiding (Move)
import Data.Monoid (Endo)
import Data.Semigroup (appEndo)

newtype Move = Move  { unMove2 :: Endo Position } deriving (Semigroup, Monoid) via (Endo Position)

runMove2 :: Move -> Position -> Position
runMove2 = appEndo . unMove2

