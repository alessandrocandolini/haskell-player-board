{-# LANGUAGE OverloadedLists #-}
module LogicSpec where

import Logic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck
import Data.Group
import Prelude hiding (Left, Right, repeat)

instance Arbitrary Direction where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Move where
  arbitrary = oneof [elements [Stay] , Step <$> arbitrary ]

instance Arbitrary Board where
  arbitrary = squareBoard <$> choose (1, 50)

instance Arbitrary Position where
  arbitrary = P <$> gen <*> gen where
     gen = choose (0,100)

spec :: Spec
spec = describe "moves" $ do

   describe "simplify" $ do
     it "should simplify a simple move tree" $
       simplify (right <> left ) `shouldBe` dontMove
     it "should simplify a less simple move tree" $
       simplify (right <> left <> dontMove) `shouldBe` dontMove
     it "should simplify a less simple move tree 2" $
       simplify (right <> dontMove <> left) `shouldBe` dontMove
     it "should simplify a less simple move tree 3" $
       simplify (right <> dontMove <> dontMove <> dontMove) `shouldBe` right
     --it "should simplify a less simple move tree 4" $
       --simplify (right <> left <> dontMove <> dontMove <> dontMove <> left) `shouldBe` left
     --it "should simplify a move tree" $
       --simplify (right <> right <> left <> up <> dontMove <> down) `shouldBe` right
   describe "runMove" $ do
     prop "always returns the input position when the move is dontmove" $
        \p -> runMove dontMove p  `shouldBe` p

     prop "always return the input position when composing a move with its inverse" $
        \p m -> runMove (invert m <> m) p  `shouldBe` p

     it "on rightDown moves a point right and then down in a single move" $
        runMove rightDown (P 7 5) `shouldBe` (P 8 4)

     it "on rightDown moves a point right and then down in a single move" $
        runMove twoRightOneDown (P 7 5) `shouldBe` (P 9 4)

   describe "move" $ do
     it "move inside the board" $
        move (squareBoard 10) right (P 5 5) `shouldBe` Just (P 6 5)


   describe "oneStep" $ do
     it "move horizontally (success)" $
        run (oneStep right) (squareBoard 10) (initial 2 3) `shouldBe` [(P 3 3)]
     it "move horizontally (failure)" $
        run (oneStep right) (squareBoard 10) (initial 9 3) `shouldBe` []
   describe "andThen" $ do
     it "move horizontally two steps (success)" $
        run (oneStep down `andThen` oneStep right `andThen` oneStep right) (squareBoard 10) (initial 5 5) `shouldBe` [(P 7 4) , (P 6 4), (P 5 4)]
     it "move horizontally two steps (failure at the next step)" $
        run (oneStep down `andThen` oneStep right `andThen` oneStep right) (squareBoard 10) (initial 9 5) `shouldBe` [(P 9 4)]
     it "move horizontally two steps (failure at the first step)" $
        run (oneStep right `andThen` oneStep down `andThen` oneStep right `andThen` oneStep right) (squareBoard 10) (initial 9 5) `shouldBe` []
     it "example 1: move horizontally two steps (failure at the next step)" $
        run (oneStep right `andThen` oneStep down `andThen` oneStep left) (squareBoard 10) (initial 8 0) `shouldBe` [(P 9 0)]
     it "example 2: example with only two (valid) moves" $
        run (oneStep up `andThen` oneStep right) (squareBoard 10) (initial 9 0) `shouldBe` [(P 9 1)]
   describe "orElse" $ do
     it "move first step works" $
        run (oneStep right `orElse` oneStep up) (squareBoard 10) (initial 8 0) `shouldBe` [(P 9 0)]
     it "move first step fails" $
        run (oneStep right `orElse` oneStep up) (squareBoard 10) (initial 9 0) `shouldBe` [(P 9 1)]
   describe "repeat" $ do
     it "move all line" $
        run (repeat (oneStep right)) (squareBoard 5) (initial 0 0) `shouldBe` [(P 4 0), (P 3 0), (P 2 0), (P 1 0)]
     it "move all line and one step down" $
        run (repeat (oneStep right) `andThen` oneStep up) (squareBoard 5) (initial 0 0) `shouldBe` [(P 4 1), (P 4 0), (P 3 0), (P 2 0), (P 1 0)]
   describe "horizontal" $ do
     it "move all board diagonal" $
        run diagonal (squareBoard 5) (initial 0 0) `shouldBe` [(P 4 4), (P 4 3) ,(P 3 3 ), (P 3 2), (P 2 2), (P 2 1), (P 1 1) , (P 1 0)]
     --it "move all board" $
        --run horizontal (squareBoard 5) (initial 0 0) `shouldBe` [(P 4 4), (P 3 4), (P 2 4),(P 1 4),(P 0 4),(P 0 3),(P 1 3),(P 2 3),(P 3 3),(P 4 3),(P 4 2),(P 3 2),(P 2 2 ),(P 1 2),(P 0 2),(P 0 1),(P 1 1),(P 2 1),(P 3 1),(P 4 1),(P 4 0),(P 3 0), (P 2 0), (P 1 0)]
