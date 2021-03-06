module Board where
import MyRandom
import Utils
import Recorridos (freeBfs)
import Debug.Trace
-- x y son las del punto a verificar x1 y1 los limites

rowDim :: (Foldable t, Num b) => t a -> b
rowDim = foldr (\ x -> (+) 1) 0

dim :: (Foldable t, Num a1, Num b) => [t a2] -> (a1, b)
dim board = (rowDim (head board), rowDim board)

totalCells :: Num a => (a, a) -> a
totalCells (x, y)= x * y

row :: (Eq t1, Num t1) => t2 -> t1 -> [t2]
row x times | times == 1 = [x]
            | otherwise = x : row x (times - 1)

buildBoard :: (Eq t1, Eq t2, Num t1, Num t2) => t1 -> t2 -> t3 -> [[t3]]
buildBoard x y t    | x == 1 = [row t y]
                    | otherwise = row t y : buildBoard (x-1) y t

--      posibles objetos en el ambiente
-- 0 -> empty
-- 1 -> obstaculos
-- 2 -> suciedad 
-- 3 -> corral
-- 4 -> niño
-- 5 -> agentes (robot d ecasa)
-- 6 -> niño encerrado en un pedazo de corral

-- printBoard
toString :: (Eq a, Num a) => a -> [Char]
toString x  | x == 0 = "  "
            | x == 1 = "OB"
            | x == 2 = "SU"
            | x == 3 = "CO"
            | x == 4 = "NÑ"
            | x == 5 = "RC"
            | otherwise = "NC"

arrayToString :: (Eq a, Num a) => [a] -> [Char]
arrayToString [] = ""
arrayToString (x:xs) = toString x ++ " " ++ arrayToString xs

boardToString :: (Eq a, Num a) => [[a]] -> [Char]
boardToString [] = ""
boardToString (x:xs) = arrayToString x ++ "\n" ++ boardToString xs

testBoard :: (Eq t1, Eq t2, Num t1, Num t2) => t1 -> t2 -> IO ()
testBoard x y = putStrLn (boardToString (buildBoard x y 0))
printBoard :: String -> IO ()
printBoard = putStrLn

-- checkForCleanHouse
percent :: Fractional a => a -> a -> a
percent p t = (p * 100)/t

checkLine :: (Eq a, Num p, Num a) => [a] -> p
checkLine [] = 0
checkLine (x:xs) | x == 0 = 1 + checkLine xs
                | otherwise = checkLine xs

checkBoard :: (Eq a, Num p, Num a) => [[a]] -> p
checkBoard = foldr ((+) . checkLine) 0 

checkHouse :: (Eq a, Num a) => [[a]] -> Bool
checkHouse board    | clean >= 60 = True
                    | otherwise = False
                where clean =  percent (checkBoard board) (totalCells (dim board))

-- this function build the simulation board with the settings received

-- para hacer al final, correr el archivo desde un .py donde se coja una seed random en python y se mande a ejecutar el programa con esa seed como paramtreo asi lidiare con el random y que el tablero sea siempre distinto
-- corral

-- step 1 => corral = 3
-- arreglar si el camino por el que se va creando el corral se ve acorralado contra una esquina y no puede terminar
-- posible solucion es revisar si se encuentra acorralado empezar a caminar en una direccion o en ves de empezar en un punto totalmente 
-- random, dividir el tablero en cuadrantes acordes a unas dimensiones que den el espacio suficiente como para que no se acorrale
-- otra solucion es realizar un bfs y quedarse con una casilla borde que no este ocupada( favorita, buscar en internet)
fillCorral board x y amount seed| amount == 0 = board
                                | otherwise = let   (newSeed, d) = myRandom seed 4
                                                    (x1, y1) = dirCoordinates x y d
                                                    col = board !! x1 
                                                    newCol = replaceNTH0 col y1 3
                                                    newBoard = replaceNTH0 board x1 newCol
                                                    in
                                                    if validPos_ x1 y1 (length board) (length (head board)) && ((board !! x1)!! y1) == 0
                                                    then
                                                        fillCorral newBoard x1 y1 (amount-1) newSeed
                                                    else
                                                        fillCorral board x y amount newSeed

step1 r board amount =  let (x, y, seed) = randomPos r board
                            -- newBoard = changePlace board x y 3
                        in freeBfs [(x,y)] board [] amount--fillCorral newBoard x y (amount-1) seed

-- step 2 => obstaculos = 1
fillObstacles board x y amount seed | amount == 0 = board
                                    | otherwise = let   (x1, y1, newSeed) = randomPos seed board
                                                        col = board !! x1 
                                                        newCol = replaceNTH0 col y1 1
                                                        newBoard = replaceNTH0 board x1 newCol
                                                    in
                                                    if validPos_ x1 y1 (length board) (length (head board)) && ((board !! x1)!! y1) == 0
                                                    then
                                                        fillObstacles newBoard x1 y1 (amount-1) newSeed
                                                    else
                                                        fillObstacles board x y amount newSeed

step2 r board amount   
    | amount == 0 = board
    | otherwise =  
        let (x, y, seed) = randomPos r board
            newBoard = changePlace board x y 1
        in fillObstacles newBoard x y (amount-1) seed

-- step 3 => suciedad = 2
fillDirty board x y amount seed | amount == 0 = board
                                    | otherwise = let   (x1, y1, newSeed) = randomPos seed board
                                                        col = board !! x1 
                                                        newCol = replaceNTH0 col y1 2
                                                        newBoard = replaceNTH0 board x1 newCol
                                                    in
                                                    if validPos_ x1 y1 (length board) (length (head board)) && ((board !! x1)!! y1) == 0
                                                    then
                                                        fillDirty newBoard x1 y1 (amount-1) newSeed
                                                    else
                                                        fillDirty board x y amount newSeed

step3 r board amount    
    | amount == 0 = board
    | otherwise =  
        let (x, y, seed) = randomPos r board
            newBoard = changePlace board x y 1
        in fillDirty newBoard x y (amount-1) seed

-- step 4 => robots = 5
fillRobots board x y amount seed    | amount == 0 = ([], board)
                                    | otherwise = 
    let (x1, y1, newSeed) = randomPos seed board
        col = board !! x1 
        newCol = replaceNTH0 col y1 5
        midBoard = replaceNTH0 board x1 newCol
        nextRobot = (x1, y1, 0)
    in
    if validPos_ x1 y1 (length board) (length (head board)) && ((board !! x1)!! y1) == 0
    then
        let (robots, newBoard) = fillRobots midBoard x1 y1 (amount-1) newSeed
        in (nextRobot : robots, newBoard)
    else
        let (robots, newBoard) = fillRobots board x y amount newSeed
        in (robots, newBoard)

step4 r board amount 
    | amount == 0 = ([], board)
    | otherwise =   let (x, y, seed) = randomPos r board
                        midBoard = changePlace board x y 5
                        firstRobot = (x, y, 0)
                        (robots, newBoard) = fillRobots midBoard x y (amount-1) seed 
                    in (firstRobot:robots, newBoard) --fillRobots newBoard x y (amount-1) seed

-- step 5 => childs = 1
fillChilds board x y amount seed    | amount == 0 = ([], board)
                                    | otherwise =   
    let (x1, y1, newSeed) = randomPos seed board
        col = board !! x1 
        newCol = replaceNTH0 col y1 4
        midBoard = replaceNTH0 board x1 newCol
        nextChild = (x1, y1, 0)
    in
    if validPos_ x1 y1 (length board) (length (head board)) && ((board !! x1)!! y1) == 0
    then
        let (childs, newBoard) = fillChilds midBoard x1 y1 (amount-1) newSeed
        in (nextChild : childs, newBoard)
    else
        let (childs, newBoard) = fillChilds board x y amount newSeed
        in (childs, newBoard)

step5 r board amount 
    | amount == 0 = ([], board)
    | otherwise =   let (x, y, seed) = randomPos r board
                        midBoard = changePlace board x y 4
                        firstChild = (x, y, 0)
                        (childs, newBoard) = fillChilds midBoard x y (amount-1) seed
                    in (firstChild:childs, newBoard)-- fillChilds newBoard x y (amount-1) seed
-- test

test = let  (robots, newBoard, childs) = makeBoard 3 60 2 2 875321564 10 10
        in printBoard (boardToString newBoard)
        -- in newBoard

-- Make Board

makeBoard o s r n seed x y = let    board = buildBoard x y 0 -- crear el board
                                    board1 = step1 seed board n -- añadir el corral
                                    board2 = step2 (seed+1) board1 o -- añadir los obstaculos
                                    board3 = step3 (seed-1) board2 s -- añadir los suciedad
                                    (robots, board4) = step4 (seed+2) board3 r -- añadir los robots
                                    (childs, newBoard) = step5 (seed-2) board4 n -- añadir los niños
                            in (robots, newBoard, childs)