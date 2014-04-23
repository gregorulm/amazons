{-# LANGUAGE DoAndIfThenElse #-}
{- |
Module      :  Main.hs
Description :  Game of the Amazons main module

Contains the main function for Game of the Amazons
-}
module Main where

import Amazons.Logic
import Amazons.TextUI

import System.Environment (getArgs)

-- |Main function for Game of the Amazons
main :: IO ()
main = do
  args <- getArgs
  if length args == 1
  then do
    board <- readBoard $ head args
    Amazons.TextUI.play board
  else Amazons.TextUI.play startingBoard
