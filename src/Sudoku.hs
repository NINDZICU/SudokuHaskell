module Sudoku where

import System.Random
import Types
import Generation

-- Генерация ячеек
generateCells :: Int -> [[Cell]]
generateCells size = undefined

generateSudoku :: [Char] -> IO [[Cell]]
generateSudoku size = do
                        putStrLn ("Generation is in progress")
                        let f = generateFlook 9
                        gougeOut getFieldCells f 0
                        {- Запускается генерация ячеек -}
                        {- Отображение сгенерированной игры -}

--вычисление решаемости
checkPossibleToSolve :: [[Cell]] -> Bool
checkPossibleToSolve cells = getSudokuSolutions cells == 1

--вычисление самих решений
getSudokuSolutions :: [[Cell]] -> Int
getSudokuSolutions cells = length (solutions cells)

--выкалывание ячеек
gougeOut :: IO [[Cell]] -> [[Int]] -> Int -> IO [[Cell]]
gougeOut cells flook iterator = do if iterator >= (gameSize ^ 2)
                                      then do
                                              c <- cells
                                              return c
                                      else do
                                              i <- getRandom 0 (gameSize - 1)
                                              j <- getRandom 0 (gameSize - 1)
                                              if (flook !! i) !! j == 0
                                                 then
                                                      do
                                                         cellsNotIO <- cells
                                                         let temp = get cellsNotIO i j
                                                         let cell = setVisible temp False
                                                         let f = replaceElementForDoubleList flook i j 1
                                                         let cells = replaceElementForDoubleList cellsNotIO i j cell
                                                         if checkPossibleToSolve cells == False
                                                            then
                                                                 do
                                                                    let cellsBack = replaceElementForDoubleList cells i j temp
                                                                    gougeOut (pure cellsBack) f (iterator + 1)
                                                            else
                                                                 gougeOut (pure cells) f (iterator + 1)
                                                 else gougeOut cells flook iterator



--заменяет элемент в двумерном списке
replaceElementForDoubleList :: [[a]] -> Int -> Int -> a -> [[a]]
replaceElementForDoubleList list index j arg = if (index < (length list) - 1) && (index /= 0)
                                        then (take index list) ++ ((replaceElement (list !! index) j arg) : drop (index + 1) list)
                                        else
                                            if (index == 0)
									           then (replaceElement (list !! index) j arg) : (drop 1 list)
                                               else (take index list) ++ [(replaceElement (list !! index) j arg)]
--заменяет элемент в списке
replaceElement :: [a] -> Int -> a -> [a]
replaceElement list index arg = if (index < length list - 1) && (index /= 0)
                                        then (take index list) ++ (arg : drop (index + 1) list)
                                        else
                                            if (index == 0)
									           then arg : (drop 1 list)
                                               else (take index list) ++ [arg]

get :: [[a]] -> Int -> Int -> a
get list i j = (list !! i) !! j

--заполнить массив непосещенных ячеек
generateFlook :: Int -> [[Int]]
generateFlook 0 = []
generateFlook size = (generateFlookLines gameSize $take gameSize $drop ((gameSize-size)*3 + div (gameSize - size) 3) $ cycle [1..gameSize]) : generateFlook(size - 1)

generateFlookLines :: Int -> [Int] -> [Int]
generateFlookLines 0 _ = []
generateFlookLines x xs = generateFlookLines (x-1) xs ++ [0]

--возвращает координаты ячеек, у которых visible = False
emptyCoordinates :: [[Cell]] -> [Coordinate]
emptyCoordinates b = [(row, col) | row <- [0..8], col <- [0..8], visible (get b col row) == False]

isPossibleMark :: Int -> Coordinate -> [[Cell]] -> Bool
isPossibleMark m (row, col) b = notInRow && notInColumn && notInBox
  where
    notInRow    = notElem m $ b `marksInRow` row
    notInColumn = notElem m $ b `marksInColumn` col
    notInBox    = notElem m $ b `marksIn3x3Box` (row, col)

--возвращает значения для столбцов
marksInRow :: [[Cell]] -> Int -> [Int]
b `marksInRow` row = [value (get b col row) | col <- [0..8], visible (get b col row) == True]

--возвращает значения для строк
marksInColumn ::  [[Cell]] -> Int -> [Int]
b `marksInColumn` col = [value (get b col row) | row <- [0..8], visible (get b col row) == True]

--возвращает значения для области 3 x 3
marksIn3x3Box :: [[Cell]] -> Coordinate -> [Int]
b `marksIn3x3Box` (row, col) = [value (get b col row) | row <- [row'..(row' + 2)], col <- [col'..(col' + 2)], visible (get b col row) == True]
  where
    row' = (row `div` 3) * 3
    col' = (col `div` 3) * 3

--возвращает ячейки с измененным значением в координатах (i,j)
copyWithMark :: Int -> Coordinate -> [[Cell]] -> [[Cell]]
copyWithMark mark (row, col) b = replaceElementForDoubleList b row col cell
  where
    cell' = get b col row
    cell = setValue cell' mark

--возвращает все решения
solutions :: [[Cell]] -> [[[Cell]]]
solutions b = solutions' (emptyCoordinates b) b
  where
    solutions' :: [Coordinate] -> [[Cell]] -> [[[Cell]]]
    solutions' []     b = [b]
    solutions' (x:xs) b = concatMap (solutions' xs) candidateBoards
      where
        candidateMarks  = [m | m <- [1..9], isPossibleMark m x b]
        candidateBoards = map (\m -> copyWithMark m x b) candidateMarks