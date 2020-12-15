import System.Random
import System.IO
import Data.Matrix
import           Data.Map   (Map)
import qualified Data.Map   as M

randInt :: Int -> Int -> IO Int

randInt low high = do
  random <- randomIO :: IO Int
  let result = low + random `mod` (high - low + 1)
  return result

data Player = X | O deriving (Bounded, Enum, Eq, Ord, Show)

data Board = Board 
  { rows    :: Int
  , columns :: Int
  , cells    :: Map (Int,Int) Player
  }

instance Show Board where
  show (Board rows columns cells) = {-"\n" ++-} unlines $ 
    [ "\n|" ++ concat [((show i) ++ "|") | i <- [0 .. columns - 1]]
    ]
    ++
    [ "|" ++ concat [[showCell (getCell row column cells), '|'] | column <- [0 .. columns - 1]] | row <- [0 .. rows - 1]
    ]
    ++
    [ "|" ++ concat [((show i) ++ "|") | i <- [0 .. columns - 1]] 
    ] 

--Return empty board with specified rows and columns
newBoard :: Int -> Int -> Board
newBoard rows columns = Board rows columns M.empty

--returns the player belonging to that cell or Nothing in case that position does not belong to the map
getCell :: Int -> Int -> Map (Int,Int) Player -> Maybe Player
getCell r c cells = M.lookup (r, c) cells

showCell :: Maybe Player -> Char
showCell Nothing = ' '
showCell (Just X) = 'X'
showCell (Just O) = 'O'

opponent :: Player -> Player
opponent X = O
opponent O = X

--return the smallest empty row, useful to know the cell where the piece will fall and stay when placed on that column
-- = -1 means the column is empty
lowestEmptyRow :: Int -> Board -> Int
lowestEmptyRow column board@(Board rows columns cells) = (check (rows - 1) column board) 
  where 
    check row column board@(Board rows columns cells)
      | (getCell row column cells) == Nothing = row
      | row < 0                               = -1
      | otherwise                             = (check (row - 1) column board)

--places that cell in the board, and returns the modified board if it was placed, or the same if it wasn't a valid move.
push :: Int -> Player -> Board -> Board
push column cell board@(Board rows columns cells)
  | not $ validMove column board     = board
  | otherwise                         = 
  Board rows columns  (M.insert (row, column) cell cells)
  where
    row =  lowestEmptyRow column board

--True if a piece can be placed at specified column, False otherwise
validMove :: Int -> Board -> Bool
validMove column board@(Board row columns _)
  | (column < 0) || (column >= columns)           = False
  | lowestEmptyRow column board == -1             = False
  | otherwise                                     = True 

--True if the board is full, False otherwise
isFull :: Board -> Bool
isFull board@(Board rows columns cells) = check 0
  where
    check c
      | c == columns                    = True
      | (lowestEmptyRow c board) == -1  = check (c+1)
      | otherwise                       = False

--returns the largest streak for the given Player from a position in the board and a function to calculate the next position 
localStreak :: Player -> Board -> (Int,Int) -> ((Int,Int) -> (Int,Int)) ->  Int
localStreak me board@(Board rows columns cells) start next = search2  start 0 0
  where
    search2 (r,c) cur maxS
      | r < 0 || c < 0 || c >= columns || r >= rows   = maxS
      | getCell r c cells == Just me                  = search2 (next (r,c)) (cur + 1) (max (cur + 1) maxS)
      | otherwise                                     = search2 (next (r,c)) 0 maxS 


--saved means there is a cell that if left empty in this turn the enemy can win on this turn, but if we place on there now, we can
--stop him from winning on next turn (so his maxStreak in thos 2 scenarios)
isSaved :: Player -> Int -> Board -> Bool
isSaved me col board@(Board rows columns cells) = 
  maxStreak (opponent me) (push col (opponent me) board) >= 4 && (maxStreak (opponent me) (push col me board)) <= 3

--returns the largest streak (connected pieces) for a player
maxStreak :: Player -> Board -> Int
maxStreak me board@(Board rows columns cells) =
  foldl max 0 [horizontal, vertical, d1a, d1b, d2a, d2b]
    where
      horizontal  = listStreak (0,0)      (add (1,0)) (add (0,1))
      vertical    = listStreak (0,0)      (add (0,1)) (add (1,0))
      d1a         = listStreak (0,0)      (add (1,0)) (add (-1,1))
      d1b         = listStreak (rows-1,0) (add (0,1)) (add (-1,1))
      d2a         = listStreak (0,0)      (add (1,0)) (add (1,1))
      d2b         = listStreak (0,0)      (add (0,1)) (add (1,1))
      listStreak start@(r, c) offset next
        | r < 0 || c < 0 || c >= columns || r >= rows = 0
        | otherwise = max (localStreak me board start next) (listStreak (offset start) offset next)
      add :: (Int,Int) -> ((Int,Int) -> (Int,Int))
      add (r,c) = \(i,j) -> (r+i, c+j)  
      --given (a,b) add returns a function that given a pair (i,j) we return (a+i,b+j)

--returns the column of the best col from all the possible moves acording to the parameters streak, and saved,
--streak is the maxStreak on the board for a given player
bestCol :: [(Int,Int,Bool)] -> Int
bestCol (move@(col, _, _):[]) = col
bestCol (move@(col, str, saved):xs) 
  | str >= 4                        = bestCol (move:tail xs) 
  | str2 >= 4                       = bestCol xs 
  | saved && saved2 && str >= str2  = bestCol (move:tail xs)  
  | saved && saved2 && str < str2   = bestCol xs
  | saved                           = bestCol (move:tail xs)
  | saved2                          = bestCol xs
  | str >= str2                     = bestCol (move:tail xs)
  | str < str2                      = bestCol xs
    where 
      (col2, str2, saved2) = head xs
      (col, str, saved) = move

--True if the player has won, False otherwise
winner :: Player -> Board -> Bool
winner me board@(Board rows columns cells) = (maxStreak me board) >= 4  

type Strategy = Board -> Player -> IO Int

--asks for a column and checks it's valid
humanStrategy :: Strategy
humanStrategy board me = do
  line <- getLine
  let col = (read line)

  if not (validMove col board) then do
    putStrLn "Invalid move, try again: "
    humanStrategy board me
  else return col 

--tries to place in random (valid) column
randomStrategy :: Strategy
randomStrategy board@(Board row columns cells) me = do
  col <- (randInt 0 (columns - 1))
  if (validMove col board) then do
    putStr $ "Random places on column " ++ (show col)
    return col
  else randomStrategy board me

--looks for the column that maximizes the Maxstreak, and if possible, tries to not lose in next move.
greedyStrategy :: Strategy
greedyStrategy board@(Board rows columns _) me = do
  let possible = [(col, str, saved) | col <- [0 .. columns - 1], validMove col board ,let str = maxStreak me (push col me board), let saved = isSaved me col board]
  let bestMove = bestCol possible
  return $ bestMove

--MiniMax with depth 4 and partial evaluation is the substraction of Maxstreaks of the 2 players.
--Could be improved with better evaluation, such as sum of all streaks.
--Reduce time complexity to explore more depth -> alpha beta pruning
smartStrategy :: Strategy
smartStrategy board@(Board rows columns _) me = do
    pair <- (maxi board 0 4)
    let col = snd pair
    return $ col
      where
        enemy = opponent me
        maxi board depth maxDepth = do
          if depth == maxDepth then return $ (evalBoard board me enemy, 0)
          else if isFull board || (winner enemy board) then maxi board maxDepth maxDepth
          else do
            (p:ossibleMoves) <- sequence $ do 
              col <- [ind | ind<-[0..columns-1], validMove ind board] 
              return (do 
                (ev, _) <- mini (push col me board) (depth+1) maxDepth
                return (ev, col))
            {-if depth < 2 then do
              putStrLn $ (show me) ++ " depth = " ++  (show depth)
              putStrLn $ show (p:ossibleMoves)
            else return () -}
            return $ foldl max p ossibleMoves
        mini board depth maxDepth = do
          if depth == maxDepth then return $ (evalBoard board me enemy, 0)
          else if isFull board || (winner me board) then mini board maxDepth maxDepth
          else do
            (p:ossibleMoves) <- sequence $ do 
              col <- [ind | ind<-[0..columns-1], validMove ind board] 
              return (do 
                (ev, _) <- maxi (push col enemy board) (depth+1) maxDepth
                return (ev, col))
            return $ foldl min p ossibleMoves
        
        evalBoard board me enemy
          | winner me board           = 100
          | winner enemy board        = -100
          | otherwise                 = maxStreak me board - (maxStreak enemy board)
      

main :: IO ()
main = do
  putStrLn "We offer this strategies: "
  putStrLn "0 -> Random Bot"
  putStrLn "1 -> Greedy Bot"
  putStrLn "2 -> Smart Bot"
  putStrLn "3 -> Human (you make the moves)"
  putStr "Insert strategy number for Player X: "
  hFlush stdout
  line <- getLine
  let indX = (read line)
  putStr "Insert strategy number for Player O: "
  hFlush stdout
  line2 <- getLine
  let indY = (read line2)
  let strats = [randomStrategy, greedyStrategy, smartStrategy, humanStrategy]
  let stratX = strats!!indX
  let stratY = strats!!indY
  play [X, O] (newBoard 6 7) [stratX, stratY]
    where 
      play [] _ _ = do
        putStrLn "END OF GAME.\n(Insert coin)"
        return ()
      play (me:enemy:[]) board [meStrategy, enemyStrat] = do
        putStr $ show board
        if isFull board then do
          putStrLn "Board is full"
          play [] board [meStrategy, enemyStrat]
        else if winner enemy board then do
          putStr $ "Player " ++ (show enemy) ++ " won\n"
          play [] board [meStrategy, enemyStrat]
        else do
          putStrLn $ "Player " ++ (show me) ++ " moves:"
          col <- meStrategy board me
          let nextBoard = (push col me board)
          play [enemy, me] nextBoard [enemyStrat, meStrategy]