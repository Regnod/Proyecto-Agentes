module Recorridos where
import Utils
import Debug.Trace
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

-- bfs para encontrar el corral
-- bfsCorral :: (Eq a, Num a) => [(Int, Int)]->[[a]]->[(Int, Int)]->a->[[(Int,Int)]]->[(Int, Int)]
bfsCorral [] _ _ _ _= []
bfsCorral nbs board visited value tracker =
    if let  (x_, y_) = head nbs
            step = indexBoard x_ y_ board
        in step /= value -- verificar que no esta ocupado para otro bfs
    then
    let (x, y) = head nbs
        step_ = indexBoard x y board
        in if step_ == 0
            then
            let newVisited = (x, y) : visited
                xnbs = getValidNeighbors (x, y) board newVisited
                newNbs = tail nbs ++ xnbs
                newTracker = addToTracker (x, y) xnbs tracker
            in bfsCorral newNbs board newVisited value newTracker
            else
            let newVisited = (x, y) : visited
                -- xnbs = getValidNeighbors (x, y) board newVisited
                newNbs = tail nbs -- ++ xnbs
                -- newTracker = addToTracker (x, y) xnbs tracker
            in bfsCorral newNbs board newVisited value tracker
    else
        getPathFromTracker tracker (head nbs) [] ++ [head nbs]

-- bfs para encontrar lo mas cercano cuando el robot esta free
bfsDumbRobotAux [] _ _ _= []
bfsDumbRobotAux nbs board visited tracker =
    if let  (x_, y_) = head nbs
            step = indexBoard x_ y_ board
        in step /= 4 && step /=2 -- verificar que no esta ocupado para otro bfs
    then
    let (x, y) = head nbs
        step_ = indexBoard x y board
        in if step_ == 0 || step_ == 6 || step_ == 3
            then
            let newVisited = (x, y) : visited
                xnbs = getValidNeighbors (x, y) board newVisited
                newNbs = tail nbs ++ xnbs
                newTracker = addToTracker (x, y) xnbs tracker
            in bfsDumbRobotAux newNbs board newVisited newTracker
            else
            let newVisited = (x, y) : visited
                -- xnbs = getValidNeighbors (x, y) board newVisited
                newNbs = tail nbs -- ++ xnbs
                -- newTracker = addToTracker (x, y) xnbs tracker
            in bfsDumbRobotAux newNbs board newVisited tracker
    else
        getPathFromTracker tracker (head nbs) [] ++ [head nbs]
bfsDumbRobot nbs board visited tracker  | checkForNotEmptyBoard board =
    let (x, y) = head nbs
        newVisited = (x, y) : visited
        xnbs = getValidNeighbors (x, y) board newVisited
        newNbs = tail nbs ++ xnbs
        newTracker = addToTracker (x, y) xnbs tracker
        path = bfsDumbRobotAux newNbs board newVisited newTracker
    in if not (null path)
        then tail path
        else []
                                        | otherwise = []
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

bfsCorralSpotAux [] _ _ tracker last = getPathFromTracker tracker last [] ++ [last]
bfsCorralSpotAux nbs board visited tracker last =
    let (x, y) = head nbs
        step_ = indexBoard x y board
        in if step_ == 3
            then
            let newVisited = (x, y) : visited
                xnbs = getValidNeighbors (x, y) board newVisited
                newNbs = tail nbs ++ xnbs
                newTracker = addToTracker (x, y) xnbs tracker
            in bfsCorralSpotAux newNbs board newVisited newTracker (x, y)
            else
            let newVisited = (x, y) : visited
                -- xnbs = getValidNeighbors (x, y) board newVisited
                newNbs = tail nbs -- ++ xnbs
                -- newTracker = addToTracker (x, y) xnbs tracker
            in bfsCorralSpotAux newNbs board newVisited tracker last
bfsCorralSpot nbs board visited tracker =
    let (x, y) = head nbs
        newVisited = (x, y) : visited
        xnbs = getValidNeighbors (x, y) board newVisited
        newNbs = tail nbs ++ xnbs
        newTracker = addToTracker (x, y) xnbs tracker
        path = bfsCorralSpotAux newNbs board newVisited newTracker (x, y)
    in path

bfsToCorral (x, y) board =
    let tracker = buildBoard (length board) (length (head board)) (-1,-1)
        path = bfsCorral [(x, y)] board [] 3 tracker
    in if not (null (tail path))
        then
            let midPath = deleteElement path (length path -1)
                (lastx, lasty) = path !! (length path -1)
                corralPath = bfsCorralSpot [(lastx, lasty)] board [] tracker
                finalPath = midPath ++ corralPath
            in (tail finalPath, False)
        else
            let
                finalPath = bfsCorralSpot [(x, y)] board [] tracker
            in (tail finalPath, True)
emptyList = []
test =
    let board =[[0,0,0,0,0,0,0,0,0,0],
                [0,0,0,5,0,0,0,0,0,0],
                [0,0,0,0,0,3,0,3,3,2],
                [0,0,0,0,3,3,3,3,3,0],
                [0,0,0,0,3,3,0,3,3,0],
                [0,0,0,1,0,0,0,3,1,0],
                [0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,5,0,0,0],
                [0,0,1,0,0,0,0,0,0,0],
                [0,0,3,3,0,0,0,0,0,0]]
        tracker = buildBoard 10 10 (-1,-1)
        start = (3,3)
        path = checkForNotEmptyBoard board
        -- path = bfsCorral [start] board [] 3 tracker
        -- path = bfsToCorral start board
        in path
    -- in let  (x, y) = last path
    --         element = indexBoard x y
    --     in element
    --[[(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)],
    -- [(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1),(-1,-1)]]