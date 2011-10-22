import List
import Control.Monad

data Cell = Empty | Circle | Cross deriving (Eq)

instance Show Cell where
  show Empty  = "_"
  show Circle = "o"
  show Cross  = "x"

generateBoard :: Int -> [[Cell]]
generateBoard size = [ [Empty | x <- [1..size]] | y <- [1..size]]

changeCell :: Int -> Int -> Cell -> [[Cell]] -> [[Cell]] 
changeCell x y cell board =
-- Translating values, coordinates shall start from one, not from zero.
  let tx = x-1
      ty = y-1
      substitutedRow = (changeAtIndex ty cell (board !! tx))
  in  changeAtIndex tx substitutedRow board

indexize :: [a] -> [(Int, a)]
indexize list = zip [0..(length list)] list

changeAtIndex :: Int -> a -> [a] -> [a]
changeAtIndex index substitution list =
  map (\ (index_x, element) ->
    if index == index_x then substitution else element
  ) $ indexize list

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
    else checkDiagonalNE player (map (\row -> take 5 row) (take 5 board)) || checkDiagonalNE player (tail board) || checkDiagonalNE player (map (\row -> tail row) board)

checkDiagonalNE :: Cell -> [[Cell]] -> Bool
checkDiagonalNE player board = checkDiagonalNW player $ transpose board

findSequence :: (Eq a) =>  [a] -> [a] -> Bool
findSequence _ [] = False
findSequence sequence list =
  if take (length sequence) list == sequence
    then True
    else findSequence sequence $ tail list

main = game "input" (generateBoard 9)

game :: String -> [[Cell]] -> IO ()
game "q" _ = print "Bye!"
game (x:',':y:[]) board = let ix = read (x:[])
                              iy = read (y:[])
                              newBoard = changeCell ix iy Circle board
                          in game "input" newBoard

game "input" board = do
  print instructions
  mapM_ (\row -> print row) board
  input <- getLine
  game input board

instructions = "Input coordinates in format: x,y. Press 'q' to exit"
