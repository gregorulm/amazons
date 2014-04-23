{- |
Module      : Amazons/Logic.hs
Description : Logic for Game of the Amazons

Game logic for Game of the Amazons
-}
module Amazons.Logic where

import Data.Maybe
import Data.Ix
import Data.List

{-
        The Game of the Amazons
        by Benjamin Liebe and Gregor Ulm

        planned features:
        - adequately represent game rules
        - support two human players
        - GUI output
        - rudimentary AI for player vs computer matches

        Important note:
        Parts of the code were adapted, but not copied, from the Sudoku
        assignment (Lab 3). This affects the following function definitions:
        - emptyBoard
        - isBoard
        - printBoard
        - readBoard
        - processInput

        Game rules, taken from [1]:
        - White moves first, and the players alternate moves thereafter.
        - Each move consists of two parts:
           a) moving one of one's own amazons one or more empty squares in
              a straight line (orthogonally or diagonally), exactly as a
              queen moves in chess; it may not cross or enter a square
              occupied by an amazon of either color or an arrow.
           b) After moving, the amazon shoots an arrow from its landing
              square to another square, using another queenlike move. This
              arrow may travel in any orthogonal or diagonal direction
              (even backwards along the same path the amazon just traveled,
              into or across the starting square if desired).
        - An arrow, like an amazon, cannot cross or enter a square where
          another arrow has landed or an amazon of either color stands.
        - The square where the arrow lands is marked to show that it can
          no longer be used.
        - The last player able to make a move wins. Draws are impossible.

        Territory and Scoring [1]:
        - The strategy of the game is based on using arrows (as well as one's
          four amazons) to block the movement of the opponent's amazons and
          gradually wall off territory, trying to trap the opponents in
          smaller regions and gain larger areas for oneself.
        - Each move reduces the available playing area.
        - Eventually each amazon finds itself in a territory blocked off from
          all other amazons.
        - The amazon can then move about its territory firing arrows until it
          no longer has any room to move. In practice the game usually ends
          when all of the amazons are in separate territories.
        - The player with the largest territory wins, as the opponent will
          fill in her own territory more quickly and therefore run out of
          space.
        - Scores are sometimes used for tie-breaking purposes in Amazons
          tournaments. Note: The number of moves remaining to a player is not
          necessarily equal to the number of empty squares in that player's       
          territories, since there could be defective territories in which
          there are fewer moves left than there are empty squares. The
          simplest such territory is three squares of the same colour, not
          in a straight line, with an amazon in the middle (for example,
          a1+b2+c1 with the amazon at b2).



        Data representation:

        The pieces are represented as follows:
                Queen White     - 'W'
                Queen Black     - 'B'
                Arrow           - 'x'
                Blank           - '.'


        The Game of the Amazons is played on a 10 x 10 board:
        
                  a b c d e f g h i j
               10
                9
                8
                7
                6
                5
                4
                3
                2
                1

        In this program, the internal representation of the board follows
        the convention of GUIs, i.e. the top-left square is (0,0),
        and the bottom-right square (9,9).

        Sources:
        [1] Wikipedia contributors, "Game of the Amazons," Wikipedia,
            http://en.wikipedia.org/wiki/Game_of_the_Amazons

-}

-- | Represents a game board with 10 x 10 fields
data Board = Board { rows :: [[Field]] }
        deriving (Show, Eq)

-- | Represents a field on the game board
data Field = Blank | Queen {player :: Player} | Arrow
        deriving (Show, Eq)

-- | Represents a player (by its color)
data Player = White | Black
        deriving (Show, Eq)

-- | Represents a position on the game board (using internal numbering, 0..9)
-- e.g. (0,0) top left corner and (9,9) bottom right corner
type Pos = (Int, Int)


-------- Representing the board --------

-- | Generates an empty board
emptyBoard :: Board
emptyBoard = Board (replicate 10 $ replicate 10 Blank)

-- | Checks whether board has 10 rows and 10 columns
isBoard :: Board -- ^ Board to check
        -> Bool
isBoard bss = length (rows bss) == 10
           && and [ length bs == 10 | bs <- rows bss]

-- | Reads a representation of a board from a text file
readBoard :: FilePath -- ^ Name of file to load board from
          -> IO Board
readBoard fname = do 
                     contents <- readFile fname
                     let bdData = processInput contents
                     case isBoard bdData of
                            False -> error "Error: Not a valid board!"
                            True  -> return bdData

-- | Turns input string into a Board representation
processInput :: String  -- ^ String to read as board
             -> Board   -- ^ Board parsed from String input
processInput inp = Board [ map createSq s | s <- words inp ]
        where createSq x =
                case x of
                        '.' -> Blank
                        'W' -> Queen White
                        'B' -> Queen Black
                        'x' -> Arrow

-- | Board only valid if exactly 4 black and 4 white queens
isNrQueensOkay :: Board  -- ^ Board to check number of queens on
               -> Bool
isNrQueensOkay bd = countPieces bd (Queen White) == 4
                           && countPieces bd (Queen Black) == 4

-- | Checks whether Board has correct dimensions and right number of queens
isBoardOkay :: Board  -- ^ Board to check for correctness
            -> Bool
isBoardOkay bd = isBoard bd && isNrQueensOkay bd

-- | Returns the count of the specified Field value
countPieces :: Board  -- ^ Board to count pieces on
            -> Field  -- ^ Field type to count
            -> Int    -- ^ Total number of pieces with type Field on Board
countPieces bd q = length $ posPieces bd q  
                

-------- Manipulating the Board --------

-- | Updates Board at given position with given value
update :: Board  -- ^ Board to update
       -> Pos    -- ^ Position that is to be updated
       -> Field  -- ^ New type of updated field
       -> Board  -- ^ Board with updated field
update bd (r, c) new_val = Board new_bd  
        where xss     = rows bd
              old_row = xss !! r
              new_row = take c old_row ++ [new_val] ++ drop (c+1) old_row
              new_bd  = take r xss ++ [new_row] ++ drop (r+1) xss

-- | Generates the starting position of the board
startingBoard :: Board -- ^ Board representing the starting position
startingBoard = putPieces boardBlackQ [(6,0),(6,9),(9,3),(9,6)] (Queen White)
        where boardBlackQ =
                putPieces emptyBoard  [(0,3),(0,6),(3,0),(3,9)] (Queen Black)

-- | Places pieces on the board
putPieces :: Board  -- ^ Board to update pieces on
          -> [Pos]  -- ^ List of positions to be updated
          -> Field  -- ^ New field type for positions
          -> Board  -- ^ Board with updated positions
putPieces bd [] _       = bd
putPieces bd (x:xs) pc  = putPieces (update bd x pc) xs pc


-------- Extracting information --------

-- | Returns content of a particular square on the board,
-- given its coordinates
squareContent :: Pos    -- ^ Field to return content from
              -> Board  -- ^ Board containing the field
              -> Field  -- ^ Type of the queried field
squareContent (r, c) bd = (rows bd !! r) !! c

-- | Returns all positions on the board
allPositions :: [Pos] -- ^ List of all possible positions of a board
allPositions = [ (x,y) | x  <- [0..9], y <- [0..9] ]

-- | Returns position of all pieces of a particular kind,
-- e.g. every Queen White
posPieces :: Board  -- ^ Board to retrieve positions from
          -> Field  -- ^ Type of field to look for
          -> [Pos]  -- ^ List of positions with field type in the board
posPieces bd piece = posPieces' bd piece allPositions

-- | Helper function for posPieces
posPieces' :: Board  -- ^ Board to retrieve positions from
           -> Field  -- ^ Type of field to look for
           -> [Pos]  -- ^ Positions not yet looked up (shrinks)
           -> [Pos]  -- ^ Positions with field type in the board (grows)
posPieces' bd pc [] = []
posPieces' bd pc (x:xs) | squareContent x bd == pc = x : posPieces' bd pc xs
                        | otherwise                = posPieces' bd pc xs

-- | Checks whether a square is empty
isSquareEmpty :: Board -- ^ Board to check square in
              -> Pos   -- ^ Position to check for emptiness
              -> Bool
isSquareEmpty bd pos = squareContent pos bd == Blank 

-- | Checks whether position is valid, i.e. between 0 and 9 on both axes
isValidPos :: Pos  -- ^ Position to check for validity
           -> Bool
isValidPos (x, y) = inRange(0,9) x && inRange(0,9) y

-- | Returns available squares for a queen to to move OR shoot an arrow to,
-- given a queen at a particular position
queenSquares :: Board  -- ^ Board to pick queen from
             -> Pos    -- ^ Position of picked queen
             -> [Pos]  -- ^ List of valid moves for queen on board
queenSquares bd pos = nub $ concatMap (takeWhile (isSquareEmpty bd)) fixedData
        where rR = rowRight pos
              rL = rowLeft pos
              cU = colUp pos
              cD = colDown pos
              d1 = diagonalNE pos
              d2 = diagonalSW pos
              d3 = diagonalNW pos
              d4 = diagonalSE pos  
              -- remove square the queen stands on:
              fixedData = map tail [rR, rL, cU, cD, d1, d2, d3, d4]
              
{-
        The following helper functions check all directions of movement and
        return the available squares, before queenSquares then truncates
        the list so that it only contains accessible squares.

        All helper functions include the square the selected queen currently
        stands on, since this is required by the function "territory" below.
-}

-- | Determines all rows in one of eight directions
rowRight, rowLeft,
  colUp, colDown,diagonalNE,
  diagonalSW, diagonalNW, diagonalSE :: Pos   -- ^ Origin
                                     -> [Pos] -- ^ Pos from origin to edge
rowRight (r,c) = [ (r,y) | y <- [c..9] ]
rowLeft (r,c)  = [ (r,y) | y <- [c, (c-1)..0] ]

colUp (r, c)   = [ (x,c) | x <- [r, (r-1)..0] ]
colDown (r,c)  = [ (x,c) | x <- [r..9] ]

diagonalNE (r,c) | r == 0 || c == 9 = [(r,c)]
                 | otherwise        = (r,c) : diagonalNE (r-1, c+1)

diagonalSW (r,c) | r == 9 || c == 0 = [(r,c)]
                 | otherwise        = (r,c) : diagonalSW (r+1, c-1)

diagonalNW (r,c) | r == 0 || c == 0 = [(r,c)]
                 | otherwise        = (r,c) : diagonalNW (r-1, c-1)

diagonalSE (r,c) | r == 9 || c == 9 = [(r,c)]
                 | otherwise        = (r,c) : diagonalSE (r+1, c+1) 


-------- Enabling Game Logic --------

-- | Checks whether the given queen has a move available
hasMove :: Board  -- ^ Board to determine moves on
        -> Pos    -- ^ Queen to determine moves
        -> Bool
hasMove bd pos = queenSquares bd pos /= []

-- | Checks whether move is legal, given start and end position
isMoveLegal :: Board  -- ^ Board to check move on
            -> Pos    -- ^ Position from which to move
            -> Pos    -- ^ Position to which to move
            -> Bool
isMoveLegal bd pos1 pos2 = pos2 `elem` queenSquares bd pos1

-- | Checks whether all queens are isolated, i.e. in their own territory
-- if so, then the game is over
queensIsolated :: Board  -- ^ Board to check isolation on
               -> Bool
queensIsolated bd = and [ x `intersect` y == [] | (x,y) <- permutations ]
        where posQueens = posPieces bd (Queen White) 
                       ++ posPieces bd (Queen Black)
              ts = [ territory bd pos  | pos <- posQueens ] 
              permutations = uniques [ (x,y) | x <- ts, y <- ts, x /= y ]

-- | Keeps only unique tuples
uniques :: Eq t => [(t,t)] -- ^ List of tuples to make unique
        -> [(t,t)]         -- ^ Unique list of tuples
uniques [] = []
uniques ((x,y):xs) | (y,x) `elem` xs  = uniques xs
                   | otherwise        = (x,y) : uniques xs


{-
        The algorithm to calculate the territory a queen occupies
        is based on the flood fill algorithm, with a scanlines approach.
        Since queens move in 8 directions, this is an 8-way flood fill.
        
        Here is an overview:
        http://en.wikipedia.org/wiki/Flood_fill

        The main idea came from this video:
        http://www.youtube.com/watch?v=6SSh-tej4lo

        The implementation is entirely our own, though.
-}

-- | Determines territory of a queen
territory :: Board  -- ^ Board to determine territory on
          -> Pos    -- ^ Position of queen to determine territory of
          -> [Pos]  -- ^ List of positions that belong to territory of queen
territory bd pos = territory' bd pos [pos] []

-- | Recursive helper function for territory
territory' :: Board -- ^ Board to determine territory on
           -> Pos   -- ^ Origin of territory determination
           -> [Pos] -- ^ Seed positions for flood fill algorithm
           -> [Pos] -- ^ Already accumulated territory positions
           -> [Pos] -- ^ Positions of territory
territory' bd (r,c) [] acc              = acc
territory' bd (r,c) (s:seeds) acc       =   
                        if (r,c) `elem` acc
                        then territory' bd s     seeds                   acc
                        else territory' bd (r,c) ((s:seeds) ++ allSeeds) acc'
        where 
              toRight    = takeWhile (goodSquare bd) (rowRight (r,c))
              toLeft     = takeWhile (goodSquare bd) (rowLeft (r,c))
              entireRow  = nub $ toRight ++ toLeft
                
              -- those are all seeds for future iterations, resulting from
              -- checks above, below, and diagonally
              seedsBelow = if (r < 9) && (r >= 0)
                           then filter (goodSquare bd)
                                        [ (x+1, y) | (x,y) <- entireRow ]
                           else [] 
                
              seedsAbove = if (r > 0) && (r <= 9)
                           then filter (goodSquare bd)
                                        [ (x-1, y) | (x,y) <- entireRow ]
                           else [] 

              -- diagonals have to be checked for every square of 'entireRow'
              allDiagonals x y = concat [ seedsNE x y,
                                          seedsSE x y,
                                          seedsNW x y,
                                          seedsSW x y ]
                        where seedsNE x y = takeWhile (goodSquare bd)
                                                      (diagonalNE (x,y))
                              seedsSE x y = takeWhile (goodSquare bd)
                                                      (diagonalSE (x,y))
                              seedsNW x y = takeWhile (goodSquare bd)
                                                      (diagonalNW (x,y))
                              seedsSW x y = takeWhile (goodSquare bd)
                                                      (diagonalSW (x,y))

              seedDiagonals = concat [allDiagonals r c | (r,c)  <- entireRow]

              -- [(r,c)] is kept around to account for the base case
              allSeeds   = concat [seedDiagonals, 
                                   seedsAbove,
                                   seedsBelow,
                                   [(r,c)]]
              acc'       = nub $ acc ++ entireRow                           

-- | Helper function for territory to limit territory
goodSquare :: Board  -- ^ Board to check position on
           -> Pos    -- ^ Position to check for territory limit
           -> Bool   -- ^ True if position is a border, otherwise False
goodSquare bd pos = squareContent pos bd /= Arrow

-- | Returns sum of defective territories in a completed game,
-- excluding squares occupied by arrows
defective :: Board  -- ^ Board to determine defective territories on
          -> [Pos]  -- ^ List of positions of defective territories
defective bd | queensIsolated bd = (allPositions \\ arrowPos)
                                   \\ queenTerritories
             | otherwise         = error "game still in progress"
        where queenPos = concat [posPieces bd pc 
                                 | pc <- [Queen White, Queen Black]]
              arrowPos = posPieces bd Arrow
              queenTerritories = concat [ territory bd p | p <- queenPos ]

-- | Computes territories; to be used with the list returned by "defective"
computeTerritories :: Board  -- ^ Board to compute on
                   -> [Pos]  -- ^ Positions of defective territories
                   -> [Pos]  -- ^ Computed territories
computeTerritories bd xs = nub $ computeTerritories' bd xs []

-- | Helper function for computeTerritories
computeTerritories' :: Board  -- ^ Board to compute on
                    -> [Pos]  -- ^ Positions of defective territories
                    -> [Pos]  -- ^ Accumulated territories
                    -> [Pos]  -- ^ Computed territories
computeTerritories' bd []     acc = acc
computeTerritories' bd (x:xs) acc = computeTerritories' bd xs 
                                    (acc ++ territory bd x)
              
-- | Scores the game, or returns Nothing if game still in progress
scoreGame :: Board             -- ^ Board to determine score from
          -> Maybe (Int, Int)  -- ^ Score result in order (White, Black)
scoreGame bd | queensIsolated bd = Just (scoreWhite, scoreBlack)
             | otherwise         = Nothing 
        where scoreWhite  = score (Queen White)
              scoreBlack  = score (Queen Black)
              score pc = sum [length $ territory bd p | p <- posPieces bd pc]

-- | Determines the winner of the game
determineWinner :: (Int, Int)  -- ^ Result to determine winner from
                -> Player      -- ^ Player that has won
determineWinner (w, b) | w > b      = White
                        | b > w     = Black
                        | otherwise = error "Not supposed to happen."
