module Child where
import Utils ( replaceNTH0, validPos_, indexBoard, findNeighbors )
import MyRandom ( myRandom )
import Board ( boardToString, printBoard )

-- states
-- 0 -> free    
-- 1 -> robot  
-- 2 -> corral 
-- 3 -> dejado por el robot

canMoveObstacle d board x y i   | validPos_ x y (length board) (length (head board)) && indexBoard x y board == 0 =(True, i) -- es que encontro un espacio en cadena para mover
                                | validPos_ x y (length board) (length (head board)) && indexBoard x y board == 1 =
    let (newx, newy) = addOnDirection d x y
    in canMoveObstacle d board newx newy (i+1)
                                | otherwise = (False, 0)

addOnDirection d x y    | d == 1 = (x+1,y)
                        | d == 2 = (x-1,y)
                        | d == 3 = (x,y+1)
                        | otherwise = (x,y-1)

getDirection x y x1 y1  | x1 == x+1 = 1
                        | x1 == x-1 = 2
                        | y1 == y+1 = 3
                        | otherwise = 4

checkNbs [] _ _ _= []
checkNbs (w:ws) board x y   | element == 0 = w : checkNbs ws board x y
                            | element == 1 &&  obstacleCanBeMove = w : checkNbs ws board x y
                            | otherwise = checkNbs ws board x y
                            where   (x1, y1) = w
                                    element = if validPos_ x1 y1 (length board) (length (head board))
                                                then indexBoard x1 y1 board
                                                else -1
                                    d = getDirection x y x1 y1
                                    (obstacleCanBeMove, index) = canMoveObstacle d board x1 y1 0

checkSides (x, y, state) board =
    let nbs = checkNbs (findNeighbors x y) board x y
    in not (null nbs)

checkCanMove state  | state == 1 = False
                    | state == 2 = False
                    | otherwise = True

childMove [] board seed newChilds = (board, newChilds, seed)
childMove (w:ws) board seed childs =
    let (willMove, newSeed) = (True, seed)--fifty seed
        (x, y, state) = w
        canMove = checkCanMove state
    in if willMove && canMove && checkSides w board
        then
            let validNbs = checkNbs (findNeighbors x y) board x y
                (nextSeed, i) = myRandom newSeed (length validNbs)
                (newx, newy) = validNbs !! i
            in if indexBoard newx newy board == 1 -- a donde se va a mover hay un obstaculo
                then -- mover los obstaculos y reemplazar el niño
                    let d = getDirection x y newx newy
                        (newBoard, newChild) = moveObstacles board d newx newy state
                        newChilds = tail childs ++ [newChild]
                    in childMove ws newBoard nextSeed newChilds
                else
                    let -- no hay cambio de estado cuando un niño se mueve solo
                        col = board !! newx
                        newCol = replaceNTH0 col newy 4
                        midBoard = replaceNTH0 board newx newCol

                        col1 = midBoard !! x
                        newCol1 = replaceNTH0 col1 y 0
                        newBoard = replaceNTH0 midBoard x newCol1

                        newChild = (newx, newy, state)
                        newChilds = tail childs ++ [newChild]
                    in childMove ws newBoard nextSeed newChilds
        else if state == 2
                -- remover el niño de la lista dado que se guardó en el corral
                -- y no podrá moverse más(probablemente nunca entre a este if :D )
                then childMove ws board newSeed (tail childs++[w])
                -- solo moverlo hacia atras porque no se puede mover por otras circunstancias
                else childMove ws board newSeed (tail childs ++ [head childs])

getPastPossWithPresentState [] _ = []
getPastPossWithPresentState _ [] = []
getPastPossWithPresentState (pastc:pastcs) (presentc:presentcs) =
    let (x, y, _) = pastc
        (_, _, state) = presentc
        newChild = (x, y, state)
    in newChild:getPastPossWithPresentState pastcs presentcs
test = let  board=[ [0,0,0,2,0,0,0,0,0,4],
                    [0,0,0,5,0,0,0,0,0,0],
                    [0,2,0,0,0,0,0,0,0,2],
                    [0,0,0,0,0,0,0,0,0,0],
                    [0,0,2,0,2,0,0,0,0,0],
                    [0,0,1,1,4,2,0,0,1,0],
                    [0,0,2,0,2,0,0,0,0,0],
                    [0,2,4,2,0,0,5,0,0,0],
                    [0,0,1,0,0,0,0,0,0,0],
                    [0,0,3,3,3,0,0,0,0,0]]
            childs = [(0,9,0), (5,4,0), (7,2,0)]
            -- (newBoard, newChilds, seed) = childMove childs board 1235798 childs
        in printBoard (boardToString board)
test1 = let board=[ [0,0,0,2,0,0,0,0,0,4],
                    [0,0,0,5,0,0,0,0,0,0],
                    [0,2,0,0,0,0,0,0,0,2],
                    [0,0,0,0,0,0,0,0,0,0],
                    [0,0,2,0,2,0,0,0,0,0],
                    [0,0,1,1,4,2,0,0,1,0],
                    [0,0,2,0,2,0,0,0,0,0],
                    [0,2,4,2,0,0,5,0,0,0],
                    [0,0,1,0,0,0,0,0,0,0],
                    [0,0,3,3,3,0,0,0,0,0]]
            childs = [(0,9,0), (5,4,0), (7,2,0)]
            -- newChilds = childMove childs board 1235798 childs
            (newBoard, newChilds, seed) = childMove childs board 1235798 childs
        in printBoard (boardToString newBoard)

moveObstacles board d x y s | d == 1 =
    let (obstacleItCanBeMove, howMany) = canMoveObstacle d board x y 0
        x1 = x + howMany
        fcol = board !! x1
        fnewCol = replaceNTH0 fcol y 1
        fBoard = replaceNTH0 board x1 fnewCol

        mcol = fBoard !! x
        mnewCol = replaceNTH0 mcol y 4
        mBoard = replaceNTH0 fBoard x mnewCol

        newCol = mBoard !! (x-1)
        newNewCol = replaceNTH0 newCol y 0
        newBoard = replaceNTH0 mBoard (x-1) newNewCol
        child = (x, y, s)
    in (newBoard, child)
                            | d == 2 =
    let (obstacleItCanBeMove, howMany) = canMoveObstacle d board x y 0
        x1 = x - howMany
        fcol = board !! x1
        fnewCol = replaceNTH0 fcol y 1
        fBoard = replaceNTH0 board x1 fnewCol

        mcol = fBoard !! x
        mnewCol = replaceNTH0 mcol y 4
        mBoard = replaceNTH0 fBoard x mnewCol

        newCol = mBoard !! (x+1)
        newNewCol = replaceNTH0 newCol y 0
        newBoard = replaceNTH0 mBoard (x+1) newNewCol
        child = (x, y, s)
    in (newBoard, child)
                            | d == 3 =
    let (obstacleItCanBeMove, howMany) = canMoveObstacle d board x y 0
        y1 = y + howMany
        fcol = board !! x
        fnewCol = replaceNTH0 fcol y1 1
        fBoard = replaceNTH0 board x fnewCol

        mcol = fBoard !! x
        mnewCol = replaceNTH0 mcol y 4
        mBoard = replaceNTH0 fBoard x mnewCol

        newCol = mBoard !! x
        newNewCol = replaceNTH0 newCol (y-1) 0
        newBoard = replaceNTH0 mBoard x newNewCol
        child = (x, y, s)
    in (newBoard, child)
                            | otherwise =
    let (obstacleItCanBeMove, howMany) = canMoveObstacle d board x y 0
        y1 = y - howMany
        fcol = board !! x
        fnewCol = replaceNTH0 fcol y1 1
        fBoard = replaceNTH0 board x fnewCol

        mcol = fBoard !! x
        mnewCol = replaceNTH0 mcol y 4
        mBoard = replaceNTH0 fBoard x mnewCol

        newCol = mBoard !! x
        newNewCol = replaceNTH0 newCol (y+1) 0
        newBoard = replaceNTH0 mBoard x newNewCol
        child = (x, y, s)
    in (newBoard, child)