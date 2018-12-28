module Main where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Sudoku
import Types
import Data.List (transpose)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

-- Это главный метод для запуска программы
main :: IO ()
main = do
    let display = InWindow "Sudoku" (screenWidth, screenHeight) (200, 200)
    let bgColor = black   -- цвет фона
    let fps     = 60      -- кол-во кадров в секунду
    playIO display bgColor fps initGame drawGame handleGame updateGame


-- =========================================
-- Модель игры
-- =========================================

-- | Виды отметок
data Mark = None| One | Two | Three | Four | Five | Six | Seven | Eight | Nine | StaticOne | StaticTwo | StaticThree | StaticFour | StaticFive | StaticSix | StaticSeven | StaticEight | StaticNine
  deriving (Eq, Show)

-- | Клетка игрового поля.
type Cell_UI = Maybe Mark                                                   

-- | Игровое поле.
type Board = [[Cell_UI]]

data MoveDirection = UpMove | DownMove | RightMove | LeftMove

-- | Состояние игры.
data Game = Game
  { gameBoard  :: Board       -- ^ Игровое поле.
  , numberValue :: Mark        -- ^ Чей ход?
  , generatedField :: Board
  , highlightedCell :: (Int, Int)
  }

-- | Начальное состояние игры.
initGame :: Game
initGame = Game
  { gameBoard  = replicate boardHeight (replicate boardWidth Nothing)
  , numberValue = One
  , generatedField = replicate boardHeight (replicate boardWidth Nothing)
  , highlightedCell = (0, 0)
  }

-- =========================================
-- Отрисовка игры
-- =========================================


-- | Отобразить игровое поле.
drawGame :: Game -> IO Picture
drawGame game = do
                   let c = fromIntegral cellSize
                   let w = fromIntegral screenWidth  / 2
                   let h = fromIntegral screenHeight / 2

                   let g = translate (-w) (-h) (scale c c (pictures [ drawGrid, drawBoard game, drawButton, drawButtonText]))
                   return g

drawButton :: Picture
drawButton = color white (translate (10.9) (6.0) $ scale 0.01 0.005 (rectangleSolid 350.0 150.0))

drawButtonText :: Picture
drawButtonText = color black (translate (9.5) (5.81) $ scale 0.004 0.005 (text "Start game"))

-- | Сетка игрового поля.
drawGrid :: Picture
drawGrid = color white (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [1..m - 1]
    vs = map (\i -> line [(i, 0), (i, m)]) [1..n - 1]

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight


-- | Нарисовать числа на игровом поле.
drawBoard :: Game -> Picture
drawBoard game = pictures (map pictures drawCells)
  where
    drawCells = map drawRow (zip [0..] (gameBoard game))
    drawRow (j, row) = map drawCellAt (zip [0..] row)
      where
        drawCellAt (i, cell) = translate (0.5 + i) (0.5 + j)
          (drawCell (estimate (gameBoard game)) cell isHighlighted)
          where
            isHighlighted = if round i == fst (highlightedCell game) && round j == snd (highlightedCell game) then True else False

-- | Нарисовать число в клетке поля (если оно там есть).
drawCell :: (Int, Int) -> Cell_UI -> Bool -> Picture
drawCell _ Nothing _ = blank
drawCell (one, two) (Just mark) highlighted
    = color markColor (drawMark mark)
    where
      markColor
       | highlighted == True = light green
       | mark == StaticOne = light orange
       | mark == StaticTwo = light orange
       | mark == StaticThree = light orange
       | mark == StaticFour = light orange
       | mark == StaticFive = light orange
       | mark == StaticSix = light orange
       | mark == StaticSeven = light orange
       | mark == StaticEight = light orange
       | mark == StaticNine = light orange
       | otherwise = white

-- | Нарисовать число.
drawMark :: Mark -> Picture
drawMark One = translate (-0.32) (-0.25) $ scale 0.01 0.005 (text "1")
drawMark StaticOne = translate (-0.32) (-0.25) $ scale 0.01 0.005 (text "1")
drawMark Two =  translate (-0.34) (-0.25) $ scale 0.008 0.005 (text "2")
drawMark StaticTwo =  translate (-0.34) (-0.25) $ scale 0.008 0.005 (text "2")
drawMark Three = translate (-0.38) (-0.25) $ scale 0.01 0.005 (text "3")
drawMark StaticThree = translate (-0.38) (-0.25) $ scale 0.01 0.005 (text "3")
drawMark Four = translate (-0.34) (-0.25) $ scale 0.008 0.005 (text "4")
drawMark StaticFour = translate (-0.34) (-0.25) $ scale 0.008 0.005 (text "4")
drawMark Five = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "5")
drawMark StaticFive = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "5")
drawMark Six = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "6")
drawMark StaticSix = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "6")
drawMark Seven = translate (-0.38) (-0.25) $ scale 0.008 0.005 (text "7")
drawMark StaticSeven = translate (-0.38) (-0.25) $ scale 0.008 0.005 (text "7")
drawMark Eight = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "8")
drawMark StaticEight = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "8")
drawMark Nine= translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "9")
drawMark StaticNine = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "9")

-- =========================================
-- Перемещение с помощью стрелок
-- =========================================

move :: Game -> MoveDirection -> (Int, Int)
move game direction = checkI (checkJ (moveTo highlighted))
  where
    highlighted = highlightedCell game
    moveTo (i, j) = case direction of
                       UpMove -> (i, j + 1)
                       DownMove -> (i, j - 1)
                       RightMove -> (i + 1, j)
                       LeftMove -> (i - 1, j)
    checkI (i, j) = case i of
               -1 -> (0, j)
               9 -> (8, j)
               otherwise -> (i, j)
    checkJ (i, j) = case j of
               -1 -> (i, 0)
               9 -> (i, 8)
               otherwise -> (i, j)
-- Подсветка
highlight :: (Int, Int) -> Game -> Game
highlight cell game = Game
  { gameBoard  = gameBoard game
  , numberValue = numberValue game
  , generatedField = generatedField game
  , highlightedCell = cell
  }

-- =========================================
-- Изменение значений с помощью клавиш 1-9
-- =========================================

markHighlighted :: Game -> Maybe Mark -> Game
markHighlighted game mark = markCell
  where
    element = ((gameBoard game) !! j) !! i
    i = fst (highlightedCell game)
    j = snd (highlightedCell game)
    markCell = case element of
                Just StaticOne -> game
                Just StaticTwo -> game
                Just StaticThree -> game
                Just StaticFour -> game
                Just StaticFive -> game
                Just StaticSix -> game
                Just StaticSeven -> game
                Just StaticEight -> game
                Just StaticNine -> game
                Just None -> Game
                       { gameBoard  = replaceElementForDoubleList (gameBoard game) j i mark
                       , numberValue = numberValue game
                       , generatedField = generatedField game
                       , highlightedCell = highlightedCell game
                       }
                otherwise -> Game
                       { gameBoard  = replaceElementForDoubleList (gameBoard game) j i mark
                       , numberValue = numberValue game
                       , generatedField = generatedField game
                       , highlightedCell = highlightedCell game
                       }

-- =========================================
-- Сообщение "You won"
-- =========================================

-- При закрытии окна GHCI тоже закрывается
wonMessage :: IO()
wonMessage = do
				display (InWindow "You won" (500, 300) (200, 200)) white drawWonMessage

drawWonMessage :: Picture
drawWonMessage = color black (translate (-100.0) (0.0) $ scale 0.4 0.5 (text "You won"))

-- на вход подается точка и 4 координаты для ограничения области
isInRect :: Point -> (Float, Float) -> (Float, Float) -> Bool
isInRect (p1, p2) x1 x2 = if p1 < fst x1 && p2 < snd x1 && p1 > fst x2 && p2 > snd x2 then True else False

isStartButton :: Point -> Bool
isStartButton point = isInRect point (307.0, 100.0) (0.0, 55.0)

-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> (Int, Int)
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize
	
-- | Обработка событий.
handleGame :: Event -> Game -> IO Game
handleGame (EventKey (Char 's') Up _ _) game = placeMark (0, 0) True game	
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game = handleLeftButtonDown mouse False game
handleGame (EventKey (SpecialKey KeyUp) Up _ _) game = castIO (highlight (move game UpMove) game)
handleGame (EventKey (SpecialKey KeyDown) Up _ _) game = castIO (highlight (move game DownMove) game)
handleGame (EventKey (SpecialKey KeyRight) Up _ _) game = castIO (highlight (move game RightMove) game)
handleGame (EventKey (SpecialKey KeyLeft) Up _ _) game = castIO (highlight (move game LeftMove) game)

-- =========================================
-- Обработчики для боковых клавиш 1-9
-- =========================================
handleGame (EventKey (SpecialKey KeyPad1) Up _ _) game = castIO (markHighlighted game (Just One))
handleGame (EventKey (SpecialKey KeyPad2) Up _ _) game = castIO (markHighlighted game (Just Two))
handleGame (EventKey (SpecialKey KeyPad3) Up _ _) game = castIO (markHighlighted game (Just Three))
handleGame (EventKey (SpecialKey KeyPad4) Up _ _) game = castIO (markHighlighted game (Just Four))
handleGame (EventKey (SpecialKey KeyPad5) Up _ _) game = castIO (markHighlighted game (Just Five))
handleGame (EventKey (SpecialKey KeyPad6) Up _ _) game = castIO (markHighlighted game (Just Six))
handleGame (EventKey (SpecialKey KeyPad7) Up _ _) game = castIO (markHighlighted game (Just Seven))
handleGame (EventKey (SpecialKey KeyPad8) Up _ _) game = castIO (markHighlighted game (Just Eight))
handleGame (EventKey (SpecialKey KeyPad9) Up _ _) game = castIO (markHighlighted game (Just Nine))

handleGame (EventKey (Char '1') Up _ _) game = castIO (markHighlighted game (Just One))
handleGame (EventKey (Char '2') Up _ _) game = castIO (markHighlighted game (Just Two))
handleGame (EventKey (Char '3') Up _ _) game = castIO (markHighlighted game (Just Three))
handleGame (EventKey (Char '4') Up _ _) game = castIO (markHighlighted game (Just Four))
handleGame (EventKey (Char '5') Up _ _) game = castIO (markHighlighted game (Just Five))
handleGame (EventKey (Char '6') Up _ _) game = castIO (markHighlighted game (Just Six))
handleGame (EventKey (Char '7') Up _ _) game = castIO (markHighlighted game (Just Seven))
handleGame (EventKey (Char '8') Up _ _) game = castIO (markHighlighted game (Just Eight))
handleGame (EventKey (Char '9') Up _ _) game = castIO (markHighlighted game (Just Nine))

handleGame _ w = castIO w

handleLeftButtonDown :: Point -> Bool -> Game -> IO Game
handleLeftButtonDown mouse isGeneration game = if fst (mouseToCell mouse) > gameSize || snd (mouseToCell mouse) > gameSize then
                            if isStartButton mouse then placeMark (0, 0) True game
							else
							   placeMark (mouseToCell mouse) False game
                       else
                          placeMark (mouseToCell mouse) False game

-- | Поставить фишку и сменить игрока (если возможно).
placeMark :: (Int, Int) -> Bool -> Game -> IO Game
placeMark (i, j) isGeneration game = do
    let place k | k == Nothing = Just (Just (changeValue None))
                | k == (Just One) = Just (Just (changeValue One))
                | k == (Just Two) = Just (Just (changeValue Two))
                | k == (Just Three) = Just (Just (changeValue Three))
                | k == (Just Four) = Just (Just (changeValue Four))
                | k == (Just Five) = Just (Just (changeValue Five))
                | k == (Just Six) = Just (Just (changeValue Six))
                | k == (Just Seven) = Just (Just (changeValue Seven))
                | k == (Just Eight) = Just (Just (changeValue Eight))
                | k == (Just Nine) = Just (Just (changeValue Nine))
                | otherwise       = Nothing
  
    if(isGeneration == True) then do
      values <- generateSudoku []
      let generates = getGenerateField values
      print(generates)
      let game = Game (generates) One (getGenerateFieldForCheck values) (0, 0)
      case modifyAt j (modifyAt i place) (gameBoard game) of
       Nothing ->castIO game 
       Just newBoard -> castIO game
        { gameBoard  = newBoard
        , numberValue = changeValue (numberValue game)
		, generatedField = generatedField game
        , highlightedCell = highlightedCell game
        }

    else do
       print(gameBoard game)
       print("----------------")
       print(generatedField game)
       print("|||||||||||||||||||||||||||||||||||||||")
       if((hasEmptyCells (boardWidth-1) (boardHeight-1) (gameBoard game)== False) &&(checkEquals (generatedField game) (gameBoard game))== True) then do
            wonMessage
            writeText("WIN")
            let place _       = Nothing
            case modifyAt j (modifyAt i place) (gameBoard game) of
                  Nothing ->castIO game  -- если поставить фишку нельзя, ничего не изменится
                  Just newBoard -> castIO game
            
	   else do
             case modifyAt j (modifyAt i place) (gameBoard game) of
               Nothing -> castIO game -- если поставить фишку нельзя, ничего не изменится
               Just newBoard -> castIO game
                { gameBoard  = newBoard
                , numberValue = changeValue (numberValue game)
				, generatedField = (generatedField game) 
                , highlightedCell = highlightedCell game
                }


-- | Сменить текущее значение
changeValue :: Mark -> Mark
changeValue One = Two
changeValue Two = Three
changeValue Three = Four
changeValue Four = Five
changeValue Five = Six
changeValue Six = Seven
changeValue Seven = Eight
changeValue Eight = Nine
changeValue Nine = One
changeValue None = One


-- | Оценить состояние игрового поля, а именно
-- вычислить сумму длин сегментов.
-- Сегменты длины 1 не учитываются при подсчёте.
estimate :: Board -> (Int, Int)
estimate _ = (0, 0)

-- | Применить преобразование к элементу списка
-- с заданным индексом. Если преобразование не удалось — вернуть 'Nothing'.
-- Иначе вернуть преобразованный список.
modifyAt :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
modifyAt _ _ []     = Nothing
modifyAt 0 f (x:xs) = case f x of
  Nothing -> Nothing
  Just y  -> Just (y : xs)
modifyAt i f (x:xs) = case modifyAt (i - 1) f xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)
	
-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> IO Game
updateGame _ w = castIO w

	
boardWidth :: Int
boardWidth = 9  -- width board

boardHeight :: Int
boardHeight = 9 -- height board

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 50

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth + 200

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight				  

--Преобразование Сell to Cell_UI
getGenerateField:: [[Cell]] -> [[Cell_UI]]
getGenerateField (x:[]) = [getLineGenerateField x]
getGenerateField (x:xs) = getLineGenerateField x : getGenerateField xs

getLineGenerateField::[Cell] -> [Cell_UI]
getLineGenerateField [] = []
getLineGenerateField (x:xs) = k : getLineGenerateField xs where 
           k | visible x == False = Nothing
             | value x == 1 = Just StaticOne
             | value x == 2 = Just StaticTwo 
             | value x == 3 = Just StaticThree
             | value x == 4 = Just StaticFour
             | value x == 5 = Just StaticFive
             | value x == 6 = Just StaticSix
             | value x == 7 = Just StaticSeven
             | value x == 8 = Just StaticEight
             | value x == 9 = Just StaticNine
			  
getGenerateFieldForCheck:: [[Cell]] -> [[Cell_UI]]
getGenerateFieldForCheck (x:[]) = [getLineGenerateFieldForCheck x]
getGenerateFieldForCheck (x:xs) = getLineGenerateFieldForCheck x : getGenerateFieldForCheck xs

getLineGenerateFieldForCheck::[Cell] -> [Cell_UI]
getLineGenerateFieldForCheck [] = []
getLineGenerateFieldForCheck (x:xs) = k : getLineGenerateFieldForCheck xs where 
            k| value x == 1 = Just StaticOne
             | value x == 2 = Just StaticTwo 
             | value x == 3 = Just StaticThree
             | value x == 4 = Just StaticFour
             | value x == 5 = Just StaticFive
             | value x == 6 = Just StaticSix
             | value x == 7 = Just StaticSeven
             | value x == 8 = Just StaticEight
             | value x == 9 = Just StaticNine

			 
castIO :: Game -> IO Game
castIO game = do 
                return game

--Проверка имеются ли еще пустые клетки на поле
hasEmptyCells ::Int -> Int -> Board -> Bool
hasEmptyCells 0 0 _ =  False
hasEmptyCells x 0 board = hasEmptyCells (x-1) (boardHeight-1) board
hasEmptyCells x y board = if(board !! x !! y /= Nothing) then hasEmptyCells x (y-1) board
                         else True
	
--Сравнение значений двух двумерных листов	
checkEquals:: [[Cell_UI]]->[[Cell_UI]] ->Bool
checkEquals [] [] = True
checkEquals (x:first) (y:second) = if (checkLineEquals x y) then checkEquals first second
                                                        else False
--Сравнение значений двух листов
checkLineEquals:: [Cell_UI] -> [Cell_UI] -> Bool
checkLineEquals [] [] = True
checkLineEquals (x:xs) (y:ys) = if (castCellToInt x /= castCellToInt y) then False
                                else checkLineEquals xs ys

--Получить значение из Cell_UI в виде Int
castCellToInt:: Cell_UI -> Int
castCellToInt (Just One) = 1
castCellToInt (Just Two) = 2
castCellToInt (Just Three) = 3
castCellToInt (Just Four) = 4
castCellToInt (Just Five) = 5
castCellToInt (Just Six) = 6
castCellToInt (Just Seven) = 7
castCellToInt (Just Eight) = 8
castCellToInt (Just Nine) = 9
castCellToInt (Just StaticOne) = 1
castCellToInt (Just StaticTwo) = 2
castCellToInt (Just StaticThree) = 3
castCellToInt (Just StaticFour) = 4
castCellToInt (Just StaticFive) = 5
castCellToInt (Just StaticSix) = 6
castCellToInt (Just StaticSeven) = 7
castCellToInt (Just StaticEight) = 8
castCellToInt (Just StaticNine) = 9
castCellToInt _ = 0


writeText:: String -> IO()
writeText a = putStrLn (a)
