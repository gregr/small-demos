module Main where
import Data.List
import Data.Maybe
import Data.Char
import System.Console.Haskeline

data Cell = Empty | X | O
  deriving Eq
instance Show Cell where
  show Empty = "-"
  show X = "X"
  show O = "O"

type Row = (Cell, Cell, Cell)
type Board = (Row, Row, Row)
data PRow = RT | RM | RB
data PCol = CL | CM | CR

tripleToList (a, b, c) = [a, b, c]

rowShow cols = intercalate " | " $ map show $ tripleToList cols
boardShow rows =
  intercalate "\n---------\n" $ map rowShow $ tripleToList rows

rowEmpty = (Empty, Empty, Empty)
boardEmpty = (rowEmpty, rowEmpty, rowEmpty)

placeInRow cell row@(Empty, m, r) CL = Just (cell, m, r)
placeInRow cell row@(l, Empty, r) CM = Just (l, cell, r)
placeInRow cell row@(l, m, Empty) CR = Just (l, m, cell)
placeInRow cell row col = Nothing

placeOnBoard cell (t, m, b) RT pcol = do
  t' <- placeInRow cell t pcol
  Just (t', m, b)
placeOnBoard cell (t, m, b) RM pcol = do
  m' <- placeInRow cell m pcol
  Just (t, m', b)
placeOnBoard cell (t, m, b) RB pcol = do
  b' <- placeInRow cell b pcol
  Just (t, m, b')

all3 pred triple = all pred $ tripleToList triple

isFullCell Empty = False
isFullCell _ = True
isFullRow = all3 isFullCell
isFull = all3 isFullRow

isWinCell choice cell = choice == cell
isWin3 choice = all3 (isWinCell choice)
isWin choice board@(t@(tl, _, tr), m@(_, mm, _), b@(bl, _, br)) =
  any (isWin3 choice) $ tripleToList board ++ columns ++ diagonals
  where
    columns = zip3 (tripleToList t) (tripleToList m) (tripleToList b)
    diagonals = [(tl, mm, br), (tr, mm, bl)]

strip = rdrop . rdrop
   where rdrop = reverse . dropWhile isSpace
getInput msg choices = loop
  where
    choiceNames = map fst choices
    prompt = msg ++ " (" ++ intercalate ", " choiceNames ++ "): "
    promptInput = runInputT defaultSettings (getInputLine prompt) >>=
      maybe promptInput (return . strip)
    loop = do
      input <- promptInput
      case lookup input choices of
        Nothing -> putStrLn "That is not one of the choices." >> loop
        Just choice -> return choice

getRow = getInput "Choose a row" [("T", RT), ("M", RM), ("B", RB)]
getCol = getInput "choose a column" [("L", CL), ("M", CM), ("R", CR)]

play player board = do
  putStrLn ""
  putStrLn $ boardShow board
  putStrLn ""
  if isWin X board
    then putStrLn "X is the winner!" >> return (Left X)
    else if isWin O board
      then putStrLn "O is the winner!" >> return (Left O)
      else if isFull board
        then putStrLn "The game is a draw!" >> return (Left Empty)
        else loop
  where
    loop = do
      row <- getRow
      col <- getCol
      case placeOnBoard player board row col of
        Nothing -> do
          putStrLn "It appears somebody has already played in that spot..."
          putStrLn $ boardShow board
          putStrLn ""
          loop
        Just board' -> do
          putStrLn "Well done, you managed to play a move."
          return $ Right board'

main = do
  putStrLn "This may or may not be a game of Tic Tac Toe..."
  loop boardEmpty
  where
    step player cont board = do
      result <- play player board
      case result of
        Left player -> return ()
        Right board' -> cont board'
    loop = step X $ step O loop
