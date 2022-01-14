module Board where
import MyRandom

-- utils
replaceNTH0 list index newItem = let (first, x:xs) = splitAt index list in first++newItem:xs

dirCoordinates :: (Eq a1, Num a1, Num a2, Num a3) => a2 -> a3 -> a1 -> (a2, a3)
dirCoordinates x y d| d == 0 = (x+1, y) 
                    | d == 1 = (x, y+1)
                    | d == 2 = (x-1, y)
                    | otherwise = (x, y-1)

-- x y son las del punto a verificar x1 y1 los limites
validPos :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> a1 -> a2 -> Bool
validPos x y x1 y1 = x >= 0 && y >= 0 && x < x1 && y < y1

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
-- 0 -> agentes (robot d ecasa)
-- 1 -> obstaculos
-- 2 -> suciedad 
-- 3 -> corral
-- 4 -> niño

-- printBoard
toString :: (Eq a, Num a) => a -> [Char]
toString x  | x == 0 = "  "
            | x == 1 = "OB"
            | x == 2 = "SU"
            | x == 3 = "CO"
            | x == 4 = "RC"
            | otherwise = "NÑ"

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

index array i = array !! i

makeSimulation settings board = 0

getRandomNumber c o s n r = c + o*2 + s*3 + n*4+ r*5

-- para hacer al final, correr el archivo desde un .py donde se coja una seed random en python y se mande a ejecutar el programa con esa seed como paramtreo asi lidiare con el random y que el tablero sea siempre distinto
-- corral

-- r es el número que se va a utilizar para el random
-- r = la suma de todas las cantidades entre 7 cada una

randomPos :: Foldable t => Int -> [t a] -> (Int, Int, Int)
randomPos r board = 
    let length0 = length board
        length1 = length (head board)
        (seed1, random1) = myRandom r length0
        (seed2, random2) = myRandom seed1 length1
    in (random1, random2, seed2)

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
                                                    if validPos x1 y1 (length board) (length (head board)) && ((board !! x1)!! y1) /= 3
                                                    then
                                                        fillCorral newBoard x1 y1 (amount-1) newSeed
                                                    else
                                                        fillCorral board x y amount newSeed

step1 r board amount =  let (x, y, seed) = randomPos r board
                            col = board !! x
                            newCol = replaceNTH0 col y 3
                            newBoard = replaceNTH0 board x newCol
                        in fillCorral newBoard x y (amount-1) seed

-- step 2 => obstaculos = 1
fillObstacles board x y amount seed | amount == 0 = board
                                    | otherwise = let   (x1, y1, newSeed) = randomPos seed board
                                                        col = board !! x1 
                                                        newCol = replaceNTH0 col y1 1
                                                        newBoard = replaceNTH0 board x1 newCol
                                                    in
                                                    if validPos x1 y1 (length board) (length (head board)) && ((board !! x1)!! y1) /= 3
                                                    then
                                                        fillObstacles newBoard x1 y1 (amount-1) newSeed
                                                    else
                                                        fillObstacles board x y amount newSeed

step2 r board amount =  let (x, y, seed) = randomPos r board
                            col = board !! x
                            newCol = replaceNTH0 col y 1
                            newBoard = replaceNTH0 board x newCol
                        in fillObstacles newBoard x y (amount-1) seed

-- test

test = let  board = buildBoard 5 5 0
            newBoard = step1 3332228 board 10
            in printBoard (boardToString newBoard)
            