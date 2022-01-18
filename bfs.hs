module Dfs where
import Utils
-- (x+1, y) 
-- (x, y+1)
-- (x-1, y)
-- (x, y-1)

row :: (Eq t1, Num t1) => t2 -> t1 -> [t2]
row x times | times == 1 = [x]
            | otherwise = x : row x (times - 1)

buildBoard :: (Eq t1, Eq t2, Num t1, Num t2) => t1 -> t2 -> t3 -> [[t3]]
buildBoard x y t    | x == 1 = [row t y]
                    | otherwise = row t y : buildBoard (x-1) y t


getNeighbors [] r c = []
getNeighbors (x:xs) r c = if validPos r c x then x: getNeighbors xs r c else getNeighbors xs r c

notElement [] _ = []
notElement (x:xs) visited   | x `notElem` visited = x: notElement xs visited
                            | otherwise = notElement xs visited

getValidNeighbors nb board visited =
    let (x, y) = nb
        r = length board
        c = length (head board)
        nbs = getNeighbors (findNeighbors x y) r c
        vnbs = notElement nbs visited
    in vnbs

-- dfs :: Num a =>[(a,a)] -> [[a]] -> [(a,a)] -> (a,a)
-- dfs :: [(Int, Int)] -> [[a]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]

dfs [] board visited (_, _) = []
dfs nbs board visited (x0, y0) = 
    if (x0, y0) /= head nbs
    then
    let (x, y) = head nbs
        newVisited = (x, y) : visited
        newNbs = getValidNeighbors (x, y) board newVisited ++ tail nbs
    in dfs newNbs board newVisited (x0, y0)
    else
        visited

-- bfs [] _ _ (_, _) = []
-- bfs nbs board visited (x0, y0) = 
--     if (x0, y0) /= head nbs
--     then
--     let (x, y) = head nbs
--         newVisited = (x, y) : visited
--         newNbs = tail nbs ++ getValidNeighbors (x, y) board newVisited
--     in bfs newNbs board newVisited (x0, y0)
--     else
--         visited
addToTracker _ [] tracker = tracker
addToTracker x' (x:xs) tracker = let    (x1, y1) = x 
                                        col = tracker !! x1 
                                        newCol = replaceNTH0 col y1 x'
                                        newTracker = replaceNTH0 tracker x1 newCol
                                in addToTracker x' xs newTracker

bfs [] _ _ _ _= []
bfs nbs board visited (x0, y0) tracker = 
    if (x0, y0) /= head nbs -- verificar que no esta ocupado para otro bfs
    then
    let (x, y) = head nbs
        newVisited = (x, y) : visited
        xnbs = getValidNeighbors (x, y) board newVisited
        newNbs = tail nbs ++ xnbs
        newTracker = addToTracker (x, y) xnbs tracker
    in bfs newNbs board newVisited (x0, y0) newTracker
    else
        getPathFromTracker tracker (x0, y0) [] ++ [(x0, y0)]


getPathFromTracker tracker (x, y) path = 
    if indexBoard x y tracker /= (-1, -1)
    then
    let x' = indexBoard x y tracker
        newPath = x':path
    in getPathFromTracker tracker x' newPath
    else path
        
-- para evitar el acorralamiento
freeBfs [] board visited = []
freeBfs nbs board visited = 
    if let (x0, y0) = head nbs in (board!!x0)!!y0 /= 0
    then
    let (x, y) = head nbs
        newVisited = (x, y) : visited
        newNbs = tail nbs ++ getValidNeighbors (x, y) board newVisited
    in freeBfs newNbs board newVisited
    else
        [head nbs]

test = 
    let board = buildBoard 5 5 0
        tracker = buildBoard 5 5 (-1,-1)
        start = (0,0)
        path = bfs [start] board [] (3,4) tracker
        in path
    -- in let  (x, y) = last path
    --         element = indexBoard x y
    --     in element