{-# LANGUAGE DoAndIfThenElse #-}
{- |
Module      :  Amazons/TextUI.hs
Description :  Text user interface for Game of the Amazons

Text user interface for Game of the Amazons
-}
module Amazons.TextUI where

import Amazons.Logic

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import Text.Printf
import System.Console.ANSI hiding (Black, White)
import qualified System.Console.ANSI as ANSI
import Text.Parsec
import Text.Parsec.String

-- | Represents the game state
data GameState = BlackWin | WhiteWin | Isolated | Undecided
        deriving (Show, Eq)

-- | A player move: (from, to, shot)
type Move = (Maybe Pos, Maybe Pos, Maybe Pos)
        
-- | Play loop of the game
play :: Board -- ^ The board to execute the play loop on
         -> IO ()
play b = do
  b' <- getMove b White
  if gameState b' == WhiteWin
  then putStrLn "Black has no moves left. White has won the game"
  else do
    b'' <- getMove b' Black
    case gameState b'' of
      BlackWin -> putStrLn "White has no moves left. Black has won the game"
      WhiteWin -> putStrLn "Black has no moves left. White has won the game"
      Isolated -> showScore b''
      Undecided -> play b''
  where
    showScore board = do
                        let score = fromJust $ scoreGame board  
                        putStrLn "Players isolated. Scoring territories:"
                        putStrLn $ uncurry (printf "White: %i Black %i") score
                        putStrLn (printf "%s has won the game"
                                  (show $ determineWinner score))

-- | Determines the game state of the board                        
gameState :: Board -> GameState
gameState b | not $ playerHasMoves b White = BlackWin
            | not $ playerHasMoves b Black = WhiteWin
            | queensIsolated b             = Isolated
            | otherwise                    = Undecided            
                             
-- | Queries a move and shot from the player
getMove :: Board    -- ^ The board representing the game progress
        -> Player   -- ^ Color of the player making the move
        -> IO Board -- ^ Board that is updated with move and shot
getMove b p = do
  printBoard b (posPieces b (Queen p))
  putStrLn (show p ++ "'s move (from, to): ")
  line <- getLine
  let
    from = readPos line 1
    to = readPos line 2
  if isJust from && isJust to &&
     isMoveLegal b (fromJust from) (fromJust to) && -- Move OK?
     (player (squareContent (fromJust from) b) == p)  -- Player color OK?
     then do
       let b' = moveAmazon b (fromJust from) (fromJust to)
       getShot b' p (fromJust to)  
  else getMove b p

-- | Queries a shot from the player
getShot :: Board     -- ^ Board to query the shot on
        -> Player    -- ^ Color of the player making the shot
        -> Pos       -- ^ Position of the queen that is shooting
        -> IO Board  -- ^ Updated board with additional arrow
getShot b p queen = do
  printBoard b (queenSquares b queen)
  putStrLn (show p ++ "'s target: ")
  line <- getLine
  let target = readPos line 1
  if isJust target && isMoveLegal b queen (fromJust target)
  then return (update b (fromJust target) Arrow)
  else getShot b p queen

-- | Checks if player has moves available
playerHasMoves :: Board   -- ^ Board to check for move
               -> Player  -- ^ Player color to check move for
               -> Bool
playerHasMoves b p = any (hasMove b) (posPieces b (Queen p))

-- | Parses a single position
pos :: Parser Pos
pos = do
  col <- oneOf ['a'..'z']
  row <- many1 digit
  return (10 - read row, ltrToInt col)
  where
    ltrToInt c = fromJust $ elemIndex(toLower c) ['a'..]
    
-- | Parses a series of positions (a move)
move :: Parser [Pos]
move = sepBy1 pos space

-- | Tries to parse n'th word from user input as Pos  
readPos :: String     -- ^ String to parse position from
        -> Int        -- ^ Get the n'th position (1..)
        -> Maybe Pos  -- ^ Parsed n'th position
readPos s n = case parse move "" s of
  Left  _    -> Nothing
  Right poss -> if (length poss < n) || not (isValidPos pos)
                then Nothing 
                else Just pos
               where pos = poss !! (n-1)
  
-- | Move amazon on the board (leaving behind Blank)
moveAmazon :: Board  -- ^ Board to perform move on
           -> Pos    -- ^ Position to move from
           -> Pos    -- ^ Position to move to
           -> Board  -- ^ Updated board
moveAmazon b from to = update (update b from Blank) to (squareContent from b)

-- | Prints colored board on the screen
printBoard :: Board   -- Board to display
           -> [Pos]   -- Positions to highlight on board
           -> IO ()
printBoard b hs = do
  clearFromCursorToScreenBeginning 
  putStr columns
  let
    idxRow = zip [0..] (rows b)
  mapM_ (printRow hs) idxRow
  putStrLn columns
  return ()
  where
    columns = "   " ++ ['a'..'j'] ++ "\n"
    printRow hs (rNum,r) = 
      do
      putStr (printf "%2i " (10-rNum))
      let
        idxField = [ ((rNum, c),f) | (c,f) <- zip [0..] r]
      mapM_ (putField hs) idxField
      putStr (printf " %2i" (10-rNum))
      putChar '\n'
      return ()
        where    
          putField hs ((r,c),f) = putColorChar (fieldToChar f) 
            (if (r,c) `elem` hs then Red else ANSI.White)

-- | Encodes a field type as a character                                 
fieldToChar :: Field  -- ^ Field to convert
            -> Char   -- ^ ASCII representation of field
fieldToChar Blank         = '.'
fieldToChar (Queen White) = 'W'
fieldToChar (Queen Black) = 'B'
fieldToChar Arrow         = 'x'

-- | Prints a colored character to the console
putColorChar :: Char        -- ^ Character to print
             -> ANSI.Color  -- ^ Color of character to print
             -> IO ()
putColorChar chr col =
  do setSGR [SetColor Foreground Vivid col]
     putChar chr
     setSGR [Reset]