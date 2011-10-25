import List
import Control.Monad

data Cell = Empty | Circle | Cross deriving (Eq)

instance Show Cell where
  show Empty  = "_"
  show Circle = "o"
  show Cross  = "x"

type Board   = [[Cell]]
type Pattern = [Cell]

generateBoard :: Int -> Board
generateBoard size = [ [Empty | x <- [1..size]] | y <- [1..size]]

getCell :: Int -> Int -> Board -> Cell
getCell x y board = (board !! x) !! y

setCell :: Int -> Int -> Cell -> Board -> Board 
setCell x y cell board =
  let newRow = (replace y cell (board !! x))
  in  replace x newRow board

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 substitute list = substitute:(tail list)
replace n substitute list = (head list):(replace (n-1) substitute (tail list))

gameFinished :: Board -> Bool
gameFinished board = (gameWon Circle board) || (gameWon Cross board)

gameWon :: Cell -> Board -> Bool
gameWon player board = any (== (take 5 (repeat player))) (possiblePatterns board)

findSequence :: (Eq a) =>  [a] -> [a] -> Bool
findSequence _ [] = False
findSequence sequence list =
  if take (length sequence) list == sequence
    then True
    else findSequence sequence $ tail list

aiResponse :: Board -> Board
aiResponse board = maximumBy (\a b -> compare (evaluateBoard a) (evaluateBoard b)) $ possibleResponses board

possibleResponses :: Board -> [Board]
possibleResponses board =
  let widths  = [0..((length board)-1)]
      heights = [0..((length $ last board)-1)]
  in  [ setCell x y Cross board | x <- widths, y <- heights, getCell x y board == Empty]

evaluateBoard :: Board -> Int
evaluateBoard board = sum (map ratePattern (possiblePatterns board))

possiblePatterns :: Board -> [Pattern]
possiblePatterns board = (horizontalPatterns board) ++ (verticalPatterns board) ++ (diagonalPatternsNW board) ++ (diagonalPatternsNE board)

horizontalPatterns :: Board -> [Pattern]
horizontalPatterns board = concat $ map takeFives board

takeFives :: [a] -> [[a]]
takeFives list
  | length list < 5  = [[]]
  | length list == 5 = [list]
  | otherwise        = [take 5 list] ++ (takeFives $ tail list)

verticalPatterns :: Board -> [Pattern]
verticalPatterns board = horizontalPatterns $ transpose board

diagonalPatternsNW :: Board -> [Pattern]
diagonalPatternsNW board =
  map extractDiagonal $ concat $ map takeFives (transpose $ map takeFives board)

extractDiagonal :: [[a]] -> [a]
extractDiagonal [[a,_,_,_,_],[_,b,_,_,_],[_,_,c,_,_],[_,_,_,d,_],[_,_,_,_,e]] =
  [a,b,c,d,e]
extractDiagonal _ = []

diagonalPatternsNE :: Board -> [Pattern]
diagonalPatternsNE board = diagonalPatternsNW $ map (\r -> reverse r) board

ratePattern :: Pattern -> Int
ratePattern pattern
  | findPattern [Circle, Circle, Circle, Circle, Empty] pattern = -10000
  | findPattern [Circle, Circle, Circle, Empty, Circle] pattern = -10000
  | findPattern [Circle, Circle, Empty, Circle, Circle] pattern = -10000
  | findPattern [Empty, Circle, Circle, Circle, Empty]  pattern = -1000
  | findPattern [Empty, Circle, Empty, Circle, Circle]  pattern = -600
  | findPattern [Circle, Empty, Circle, Circle, Empty]  pattern = -600
  | findPattern [Circle, Circle, Empty]                 pattern = -10
  | findPattern [Circle, Empty, Circle, Empty]          pattern = -10
  | findPattern [Cross, Cross, Cross, Cross, Cross] pattern = 100000000
  | findPattern [Cross, Cross, Cross, Cross, Empty] pattern = 5000
  | findPattern [Cross, Cross, Cross, Empty, Cross] pattern = 5000
  | findPattern [Cross, Cross, Empty, Cross, Cross] pattern = 5000
  | findPattern [Empty, Cross, Cross, Cross, Empty] pattern = 50
  | findPattern [Empty, Cross, Empty, Cross, Cross] pattern = 24
  | findPattern [Cross, Empty, Cross, Cross, Empty] pattern = 24
  | findPattern [Cross, Cross, Empty]               pattern = 5
  | findPattern [Cross, Empty, Cross, Empty]        pattern = 5
  | findPattern [Cross, Empty, Empty, Empty, Empty] pattern = 1
  | otherwise = 0

findPattern :: Pattern -> Pattern -> Bool
findPattern pattern list 
  | length pattern == 5 = (list == pattern) || (list == (reverse pattern))
  | otherwise = (findSequence pattern list) || (findSequence (reverse pattern) list)


main = getUserInput (generateBoard 11)

printBoard :: Board -> IO ()
printBoard board =
  let indexedBoard = zip [0..(length board)] board
  in  do
    -- Numbers on top of the board, i.e.: 1 2 3 4 ...
    mapM_ (\c -> putChar c) "  "
    mapM_ (\i -> do
      putChar $ last $ show i
      putChar ' '
      ) [1..(length board)]
    putChar '\n'
    -- Board rows.
    mapM_ (\(i,row) -> print $ (show row) ++ " " ++ (show $ 1+i)) indexedBoard

getUserInput :: Board -> IO ()
getUserInput board = do
  if gameFinished board
    then declareWinner board
    else do
      print instructions
      printBoard board
      input <- getLine
      processUserInput input board

declareWinner :: Board -> IO ()
declareWinner board = do
  printBoard board
  if gameWon Circle board
    then
      print "End of game, you win!"
    else
      print "End of game, the computer has won."
  
processUserInput :: String -> Board -> IO ()
processUserInput "q" _ = print "Bye!"
processUserInput input board =
  let (x,y) = break (==',') input
      -- Translating values, coordinates will start from one, not from zero.
      ix = (read x) -1
      iy = (read $ tail y) -1
      userResponse = setCell ix iy Circle board
      newBoard     = if gameFinished userResponse
          then userResponse
          else aiResponse userResponse
  in getUserInput newBoard

instructions = "Input coordinates in format: x,y. Press 'q' to exit"
