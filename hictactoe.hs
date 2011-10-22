import List
import Control.Monad

data Cell = Empty | Circle | Cross deriving (Eq)

instance Show Cell where
  show Empty  = "_"
  show Circle = "o"
  show Cross  = "x"

generateBoard :: Int -> [[Cell]]
generateBoard size = [ [Empty | x <- [1..size]] | y <- [1..size]]

getCell :: Int -> Int -> [[Cell]] -> Cell
getCell x y board = (board !! x) !! y

setCell :: Int -> Int -> Cell -> [[Cell]] -> [[Cell]] 
setCell x y cell board =
  let substitutedRow = (changeAtIndex y cell (board !! x))
  in  changeAtIndex x substitutedRow board

indexize :: [a] -> [(Int, a)]
indexize list = zip [0..(length list)] list

changeAtIndex :: Int -> a -> [a] -> [a]
changeAtIndex index substitution list =
  map (\ (index_x, element) ->
    if index == index_x then substitution else element
  ) $ indexize list

gameFinished :: [[Cell]] -> Bool
gameFinished board = (gameWon Circle board) || (gameWon Cross board)

gameWon :: Cell -> [[Cell]] -> Bool
gameWon player board = checkHorizontal player board || checkVertical player board || checkDiagonalNE player board || checkDiagonalNW player board

checkHorizontal :: Cell -> [[Cell]] -> Bool
checkHorizontal player board =
  any (\ row -> findSequence ( take 5 $ repeat player) row ) board
  
checkVertical :: Cell -> [[Cell]] -> Bool
checkVertical player board = checkHorizontal player $ transpose board

checkDiagonalNW :: Cell -> [[Cell]] -> Bool
checkDiagonalNW player [[a,_,_,_,_],[_,b,_,_,_],[_,_,c,_,_],[_,_,_,d,_],[_,_,_,_,e]] =
  player == a && a == b && b == c && c == d && d == e
checkDiagonalNW player board =
  if (length board) < 5 || any (\row -> (length row) < 5) board
    then False
    else checkDiagonalNW player (map (\row -> take 5 row) (take 5 board)) || checkDiagonalNW player (tail board) || checkDiagonalNW player (map (\row -> tail row) board)

checkDiagonalNE :: Cell -> [[Cell]] -> Bool
checkDiagonalNE player board = checkDiagonalNW player $ map (\r -> reverse r) board

findSequence :: (Eq a) =>  [a] -> [a] -> Bool
findSequence _ [] = False
findSequence sequence list =
  if take (length sequence) list == sequence
    then True
    else findSequence sequence $ tail list

aiResponse :: [[Cell]] -> [[Cell]]
aiResponse board = last $ possibleResponses board

possibleResponses :: [[Cell]] -> [[[Cell]]]
possibleResponses board =
  let widths  = [0..((length board)-1)]
      heights = [0..((length $ last board)-1)]
  in  [ setCell x y Cross board | x <- widths, y <- heights, getCell x y board == Empty]



main = getUserInput (generateBoard 11)

printBoard :: [[Cell]] -> IO ()
printBoard board =
  let indexedBoard = indexize board
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

getUserInput :: [[Cell]] -> IO ()
getUserInput board = do
  if gameFinished board
    then declareWinner board
    else do
      print instructions
      printBoard board
      input <- getLine
      processUserInput input board

declareWinner :: [[Cell]] -> IO ()
declareWinner board = do
  printBoard board
  if gameWon Circle board
    then
      print "End of game, you win!"
    else
      print "End of game, the computer has won."
  
processUserInput :: String -> [[Cell]] -> IO ()
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
