module Utils where
import MyRandom
replaceNTH0 list index newItem =
    if index /= length list
    then let (first, x:xs) = splitAt index list in first++newItem:xs
    else let (first, xs) = splitAt index list in first++[newItem]

validPos x1 y1 (x, y) = x >= 0 && y >= 0 && x < x1 && y < y1
validPos_ :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> a1 -> a2 -> Bool
validPos_ x y x1 y1 = x >= 0 && y >= 0 && x < x1 && y < y1

indexBoard x y board = board !! x !! y

dirCoordinates :: (Eq a1, Num a1, Num a2, Num a3) => a2 -> a3 -> a1 -> (a2, a3)
dirCoordinates x y d| d == 0 = (x+1, y)
                    | d == 1 = (x, y+1)
                    | d == 2 = (x-1, y)
                    | otherwise = (x, y-1)

randomPos :: Foldable t => Int -> [t a] -> (Int, Int, Int)
randomPos r board =
    let length0 = length board
        length1 = length (head board)
        (seed1, random1) = myRandom r length0
        (seed2, random2) = myRandom seed1 length1
    in (random1, random2, seed2)

fifty seed = let (newSeed, r) = myRandom seed 100
            in (r < 50, newSeed)

findNeighbors x y =
    let (x1, y1) =  (x+1, y)
        (x2, y2) =  (x, y+1)
        (x3, y3) =  (x-1, y)
        (x4, y4) =  (x, y-1)
    in  [(x1, y1), (x2, y2), (x3, y3), (x4, y4)]

find _ _ [] _ = ((-1, -1, -1), -1)
find x y (o:os) i   | x1 == x && y1 == y = (o,i)
                    | otherwise = find x y os (i+1)
                    where (x1, y1, _) = o

removeXY x y list =
    let (_, index) = find x y list 0
    in  if index /= length list
        then let (first, x:xs) = splitAt index list in first++xs
        else let (first, xs) = splitAt index list in first

changePlace board x y element =
    let col = board !! x
        newCol = replaceNTH0 col y element
        newBoard = replaceNTH0 board x newCol
    in newBoard

deleteElement list index =
    if index == 0 then tail list
    else
        let (a, b) = splitAt index list
        in if not (null b) then a ++ tail b else list

checkForNotEmptyBoardAux board x y  | validPos_ x y c l && (spot == 2 || spot == 4) = True
                                    | otherwise =
    let l = length (head board)
        c = length board
    in c /= x && (if l == y 
                    then checkForNotEmptyBoardAux board (x+1) 0 
                    else  checkForNotEmptyBoardAux board x (y+1))
                                    where   spot = indexBoard x y board
                                            l = length (head board)
                                            c = length board
checkForNotEmptyBoard board = checkForNotEmptyBoardAux board 0 0

makeRobotIntel [] = [] 
makeRobotIntel ((x, y, state):rs) = (x,y,state, (-1,-1)):makeRobotIntel rs 

findTargetByR _ [] _ = ((-1, -1, (-1,-1)), -1)
findTargetByR (x, y) (t:ts) i  | x1 == x && y1 == y = (t, i)
                            | otherwise = findTargetByR (x, y) ts (i+1)
                    where (_, _, (x1, y1)) = t