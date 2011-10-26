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

verticalPatterns :: Board -> [Pattern]
verticalPatterns board = horizontalPatterns $ transpose board

diagonalPatternsNW :: Board -> [Pattern]
diagonalPatternsNW board =
  map extractDiagonal $ concat $ map takeFives (transpose $ map takeFives board)

diagonalPatternsNE :: Board -> [Pattern]
diagonalPatternsNE board = diagonalPatternsNW $ map (\r -> reverse r) board

ratePattern :: Pattern -> Int
ratePattern pattern =
  let patterns = [
        ([Circle, Circle, Circle, Circle, Empty ], -100000    ),
        ([Circle, Circle, Circle, Empty, Circle ], -100000    ),
        ([Circle, Circle, Empty, Circle, Circle ], -100000    ),
        ([Empty, Circle, Circle, Circle, Empty  ], -1000      ),
        ([Empty, Circle, Empty, Circle, Circle  ], -600       ),
        ([Circle, Empty, Circle, Circle, Empty  ], -600       ),
        ([Circle, Circle, Empty                 ], -10        ),
        ([Circle, Empty, Circle, Empty          ], -10        ),
        ([Cross, Cross, Cross, Cross, Cross     ],  100000000 ),
        ([Cross, Cross, Cross, Cross, Empty     ],  5000      ),
        ([Cross, Cross, Cross, Empty, Cross     ],  5000      ),
        ([Cross, Cross, Empty, Cross, Cross     ],  5000      ),
        ([Empty, Cross, Cross, Cross, Empty     ],  50        ),
        ([Empty, Cross, Empty, Cross, Cross     ],  24        ),
        ([Cross, Empty, Cross, Cross, Empty     ],  24        ),
        ([Cross, Cross, Empty                   ],  5         ),
        ([Cross, Empty, Cross, Empty            ],  5         ),
        ([Cross, Empty, Empty, Empty, Empty     ],  1         ) ]
      findPatternWithMirrored p s = findPattern p s || findPattern (reverse p) s
      getPatternEntry p = find (\(r,i)->findPatternWithMirrored r p) patterns
      extractRating (Just (a,b)) = b
      extractRating Nothing = 0
  in  extractRating $ getPatternEntry pattern

takeFives :: [a] -> [[a]]
takeFives list
  | length list >= 5 = [take 5 list] ++ (takeFives $ tail list)
  | otherwise        = []

extractDiagonal :: [[a]] -> [a]
extractDiagonal [[a,_,_,_,_],[_,b,_,_,_],[_,_,c,_,_],[_,_,_,d,_],[_,_,_,_,e]] =
  [a,b,c,d,e]
extractDiagonal _ = []

findPattern :: (Eq a) =>  [a] -> [a] -> Bool
findPattern _ [] = False
findPattern sequence list =
  if length sequence == length list
    then sequence == list
    else if take (length sequence) list == sequence
      then True
      else findPattern sequence $ tail list


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
