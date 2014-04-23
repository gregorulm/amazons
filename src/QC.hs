{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Amazons/QC.hs
Description : QuickCheck Properties for Game of the Amazons

QuickCheck Properties for Game of the Amazons
-}
module QC where

import Amazons.Logic
import Amazons.TextUI
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Data.Maybe

-- | Runs automatically all prop_* functions in this module.
-- Inspired by http://stackoverflow.com/a/13535259
main :: IO ()
main = do
  passed <- $quickCheckAll
  if passed then putStrLn "All tests passed."
            else putStrLn "Some tests failed."

-- Make Board an arbitrary instance
instance Arbitrary Board where
  arbitrary =
    do rows <- suchThat (
                 sequence [
                   sequence [field | j <- [1..10] ] | i <- [1..10]
                 ]
                )
               (isNrQueensOkay . Board)
       return (Board rows)

-- |Make Field an arbitrary instance
instance Arbitrary Field where
  arbitrary = field
  
-- |Generates an arbitrary game field
field :: Gen Field
field = frequency
         [(8, return Blank),
          (84, return Arrow),
          (4, return $ Queen White),
          (4, return $ Queen Black)]
          
-- Properties for Amazons.Logic

-- |isBoard must approve all generated boards
prop_isBoard :: Board -> Bool
prop_isBoard = isBoard

-- |countPieces must achieve plausible sums
prop_countPieces :: Board -> Field -> Bool
prop_countPieces b f = countPieces b f == total - remaining
  where
    others = [Arrow, Blank, Queen White, Queen Black] \\ [f]
    total = 10*10
    remaining = sum $ map (countPieces b) others
    
-- |An updated board must still be a valid board
prop_updateValid :: Board -> Pos -> Field -> Bool
prop_updateValid b (x,y) f = isBoard (update b (x',y') f)
  where
    x' = x `mod` 10
    y' = x `mod` 10

-- |An updated followed by an reverse update must yield the original board
prop_updateReverse :: Board -> Pos -> Field -> Bool
prop_updateReverse b (x,y) f' = update (update b (x',y') f') (x',y') f == b
  where
    f = squareContent (x',y') b
    x' = x `mod` 10
    y' = x `mod` 10

-- |All updates must indeed get written to the board
prop_putPieces :: Board -> [Pos] -> Field -> Bool
prop_putPieces b ps f = all predicate (posPieces b f)
  where predicate p = squareContent p b == f
  
-- |Identified territories may not contain arrows
prop_territoryNoArrows :: Board -> Property
prop_territoryNoArrows b = queensIsolated b ==> all noArrow queenPositions
  where
    noArrow pos = all noArrowPred (territory b pos)
    noArrowPred pos = squareContent pos b /= Arrow
    queenPositions = posPieces b (Queen Black) ++ posPieces b (Queen White)

-- | All allowed target positions must be within the territory
prop_territoryValidMove :: Board -> Int -> Property
prop_territoryValidMove b i = hasQueens 
                          ==> all isInTerritory (queenSquares b arbQueen)
  where
    hasQueens = not (null queens)
    arbQueen = queens !! (i `mod` length queens)
    queens = posPieces b (Queen White) ++ posPieces b (Queen Black)
    isInTerritory pos = pos `elem` territory b pos
    
-- |All boards with isolated queens must be scorable
prop_scoreGameIsolation :: Board -> Property
prop_scoreGameIsolation b = queensIsolated b ==> isJust $ scoreGame b

-- |Scores may not exceed the total number of blank fields and Queens
prop_scoreGameTotal :: Board -> Property
prop_scoreGameTotal b = isJust score ==> fst score' + snd score' <= 
                        length (posPieces b Blank) + 8
  where score  = scoreGame b
        score' = fromJust score

-- |Defective territories must not overlap with "regular" territories.
-- This property actually helped to find a bug in territory'.
prop_defectiveOverlap :: Board -> Property
prop_defectiveOverlap bd = queensIsolated bd 
                       ==> allPositions \\ ((allPositions \\ defective bd) 
                        ++ computeTerritories bd (defective bd)) == []
                        
                        