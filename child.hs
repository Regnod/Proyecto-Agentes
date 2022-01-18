module Child where
import Utils
import MyRandom
-- parametros 
-- lista de posiciones de los niños
-- el tablero
-- un seed para usar random
-- nuevo array de niños
data Child = Child Int Int Int
-- checkNbs :: Num a => [(a, a)] -> [[a]]->[(a, a)]
-- TODO: revisar cuando es un obstaculo si se puede mover ese obstaculo


-- states
-- free -> 0 
-- robot -> 1
-- corral -> 2
-- TODO: terminar este metodo :)
canMoveObstacle = True

checkNbs [] _ = []
checkNbs (x:xs) board   | element == 0 = x: checkNbs xs board
                        | element == 1 && canMoveObstacle = x: checkNbs xs board
                        | otherwise = checkNbs xs board
                        where   (x1, y1) = x
                                element = indexBoard x1 y1 board

checkSides (x, y) board =
    let nbs = checkNbs (findNeighbors x y) board
    in not (null nbs)

-- TODO: añadir un estado de si esta cargado o no 
checkCanMove (x, y) board   | indexBoard x y board == 3 = False
                            | indexBoard x y board == 5 = False
                            | otherwise = True

-- childMove :: (Eq a, Num a)  => [(Int, Int)] -> [[a]] -> Int -> [a] -> ([[a]], [a])
childMove [] board seed newChilds = (board, newChilds, seed)
childMove (w:ws) board seed childs =
    let (willMove, newSeed) = fifty seed
        (x, y) = w -- TODO: fix later if using types
        canMove = checkCanMove w board
        validSides = checkSides w board
    in if willMove && canMove && validSides
        then
            let validNbs = checkNbs (findNeighbors x y) board
                (newSeed, d) = myRandom seed (length validNbs)
                (newx, newy) = validNbs !! d
                -- TODO: hacer un if para mover un objeto si es necesario, 
                -- también añadir un bool de si ese camino tiene objeto
                -- para saber si hay que correr ese objeto y cuantos hay que correr
                (oldx, oldy, state) = head childs
                -- no hay cambio de estado cuando un niño se mueve solo
                newChild = (newx, newy, state)
                newChilds = tail childs ++ [newChild]
            in childMove ws board newSeed newChilds
        else let(oldx, oldy, state) = head childs
            in  if state == 2
                -- remover el niño de la lista dado que se guardó en el corral
                -- y no podrá moverse más
                then childMove ws board newSeed (tail childs)
                else childMove ws board newSeed (tail childs ++ [head childs])
