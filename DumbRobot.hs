module DumbRobot where
import Utils
import MyRandom
import Recorridos 
import Debug.Trace

-- states
-- free -> 0
-- holding baby -> 1
-- over a dirt -> 2
-- leaving child -> 3
-- over a corral -> 4

-- oldSpot es lo que había antes en esa posición
spotNewPosition robot board (newx, newy) oldSpot childs | spot == 2 = -- camina hacia una suciedad
    -- no va a pasar que llegue a caminar sobre una suciedad con un niño cargado
    -- en este caso se que el estado es 0
    let (oldx, oldy, state) = robot
        col = board !! oldx
        newCol = replaceNTH0 col oldy oldSpot
        inBetweenBoard = replaceNTH0 board oldx newCol
        
        col1 = inBetweenBoard !! newx
        newCol1 = replaceNTH0 col1 newy 5
        newBoard = replaceNTH0 inBetweenBoard newx newCol1

        newRobot = (newx, newy, 2)
    in  (newRobot, newBoard, childs)
                                                        | spot == 4 = -- camina hacia un niño
    let (oldx, oldy, state) = robot
        col = board !! oldx
        newCol = replaceNTH0 col oldy oldSpot
        inBetweenBoard = replaceNTH0 board oldx newCol
        
        col1 = inBetweenBoard !! newx
        newCol1 = replaceNTH0 col1 newy 5
        newBoard = replaceNTH0 inBetweenBoard newx newCol1

        newRobot = (newx, newy, 1)
        ((childx, childy, childState), index) = find newx newy childs 0
        newChild = (childx, childy, 1)
        newChilds = replaceNTH0 childs index newChild
    in  (newRobot, newBoard, newChilds)
                                                        | spot == 3 && state == 1 = -- camina hacia una casilla corral 
    let (oldx, oldy, state) = robot
        col = board !! oldx
        newCol = replaceNTH0 col oldy oldSpot
        inBetweenBoard = replaceNTH0 board oldx newCol
        
        col1 = inBetweenBoard !! newx
        newCol1 = replaceNTH0 col1 newy 5
        newBoard = replaceNTH0 inBetweenBoard newx newCol1

        newRobot = (newx, newy, 3)   
        ((childx, childy, childState), index) = find oldx oldy childs 0
        newChild = (newx, newy, 2)
        newChilds = replaceNTH0 childs index newChild
        -- -- eliminar ese niño de la lista de niños
        -- newChild = (childx, childy, 2)
        -- newChilds = replaceNTH0 childs index newChild
    in  (newRobot, newBoard, newChilds)
                                                        | spot == 3 && state /= 1 = -- camina hacia una casilla corral 
    let (oldx, oldy, state) = robot
        col = board !! oldx
        newCol = replaceNTH0 col oldy oldSpot
        inBetweenBoard = replaceNTH0 board oldx newCol
        
        col1 = inBetweenBoard !! newx
        newCol1 = replaceNTH0 col1 newy 5
        newBoard = replaceNTH0 inBetweenBoard newx newCol1

        newRobot = (newx, newy, 4)
        -- newChilds = removeXY oldx oldy childs
        -- ((childx, childy, childState), index) = find oldx oldy childs 0
        -- newChild = (childx, childy, 2)
        -- newChilds = replaceNTH0 childs index newChild
    in  (newRobot, newBoard, childs)
                                                        | otherwise = -- camina normalmente
    
    let (oldx, oldy, state) = robot
    in if state == 3 || state == 4
        then
            let col = board !! oldx
                newCol = replaceNTH0 col oldy oldSpot
                inBetweenBoard = replaceNTH0 board oldx newCol

                col1 = inBetweenBoard !! newx
                newCol1 = replaceNTH0 col1 newy 5
                newBoard = replaceNTH0 inBetweenBoard newx newCol1
                newRobot = (newx, newy, 0)
            in (newRobot, newBoard, childs)
        else
            let col = board !! oldx
                newCol = replaceNTH0 col oldy oldSpot
                inBetweenBoard = replaceNTH0 board oldx newCol
                
                col1 = inBetweenBoard !! newx
                newCol1 = replaceNTH0 col1 newy 5
                newBoard = replaceNTH0 inBetweenBoard newx newCol1
                newRobot = (newx, newy, state)
            in (newRobot, newBoard, childs)
                                                        where   spot = indexBoard newx newy board
                                                                (_, _, state) = robot

cleanDirt robot = 
    let (x, y, _) = robot
    in (x,y, 0)

-- si al entrar a moveRobot no tiene estado 1 siempre se mueve solo 1 casilla por lo que longitude siempre es 1
-- osea que no hace falta llamar recursivo despues de moverse esa ves
-- la diferencia de los casos es el valor que se deja en el lugar después de moverse
moveRobot robot board path longitude childs | longitude > 0 && not (null path) && state == 3 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 6 childs
    in (newBoard, newRobot, newChilds)
                                        | longitude > 0 && not (null path) && state == 4 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 3 childs
    in (newBoard, newRobot, newChilds)-- moveRobot newRobot newBoard (tail path) (longitude-1) newChilds
                                        | longitude > 0 && not (null path) && state == 0 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 0 childs
    in (newBoard, newRobot, newChilds)
                                        | longitude > 0 && not (null path) && state == 1 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 0 childs
        (_, _, currentState) = newRobot
    in if currentState == 3 
        then (newBoard, newRobot, newChilds) 
        else moveRobot newRobot newBoard (tail path) (longitude-1) newChilds
                                        | otherwise = (board, robot, childs)
                                    where (oldx, oldy, state) = robot

-- este metodo mueve un solo robot y devuelve el board y el robot luego de realizar el movimiento
moveDumbRobot [] board  robots childs = (board, robots, childs)--(board, robots, childs)
moveDumbRobot (r:rs) board  robots childs   | state == 2 = --arriba de una suciedad
    let newRobot = cleanDirt r
        newRobots = tail robots ++ [newRobot]
    -- in (board, [], childs)
    in moveDumbRobot rs board newRobots childs
                                    | state == 1 = -- significa que carga un niño
    let (x, y, _) = r
        tracker = buildBoard (length board) (length (head board)) (-1,-1)
        changedBoard = changePlace board x y 0
        (path, c) = bfsToCorral (x, y) changedBoard
        (newRobot, newBoard, newChilds) =  moveWithChild r board path childs c
        newRobots = tail robots ++ [newRobot]
    in moveDumbRobot rs newBoard newRobots newChilds
    -- let (x, y, _) = r
    --     tracker = buildBoard (length board) (length (head board)) (-1,-1)
    --     changedBoard = changePlace board x y 0
    --     nearestPathToCorral = bfsCorral [(x,y)] changedBoard [] 3 tracker
    -- in if not (null nearestPathToCorral)
    --     then 
    --         let (newBoard, newRobot, newChilds) = moveRobot r board (tail nearestPathToCorral) 2 childs
    --             newRobots = tail robots ++ [newRobot]
    --         in moveDumbRobot rs newBoard newRobots newChilds
    -- -- in (board, nearestPathToCorral, childs)
    --     else 
    --         let newRobots = tail robots ++ [r]
    --         in moveDumbRobot rs board newRobots childs
                                    | otherwise = -- significa que no carga un niño
    -- ver a donde me muevo y moverme
    let (x, y, _) = r
        tracker = buildBoard (length board) (length (head board)) (-1,-1)
        nearestPath = bfsDumbRobot [(x, y)] board [] tracker
        (newBoard, newRobot, newChilds) = moveRobot r board nearestPath 1 childs
        newRobots = tail robots ++ [newRobot]
    -- in (board, nearestPath, childs)
    in moveDumbRobot rs newBoard newRobots newChilds
                                    where (_, _, state) = r

moveWithChild r board path childs c | length path > 2 =
    let (x, y, state) = r
        (newx, newy) = path !! 1
        midBoard = changePlace board newx newy 5
    in if c 
        then
            let newBoard = changePlace midBoard x y 3
                newRobot = (newx, newy, state)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 1)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
        else
            let newBoard = changePlace midBoard x y 0
                newRobot = (newx, newy, state)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 1)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
                                    | length path == 2 =
    let (x, y, state) = r
        (newx, newy) = path !! 1
        midBoard = changePlace board newx newy 5 
    in if c 
        then
            let newBoard = changePlace midBoard x y 3
                newRobot = (newx, newy, 3)
                -- newChilds = removeXY x y childs
                ((childx, childy, childState), index) = trace (show (find x y childs 0)) find x y childs 0
                newChild = (newx, newy, 2)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
        else
            let newBoard = changePlace midBoard x y 0
                newRobot = (newx, newy, 3)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 2)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
                                    | length path == 1 =
    let (x, y, state) = r
        (newx, newy) = head path
        midBoard = changePlace board newx newy 5
    in if c 
        then
            let newBoard = changePlace midBoard x y 3
                newRobot = (newx, newy, 3)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 2)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
        else
            let newBoard = changePlace midBoard x y 0
                newRobot = (newx, newy, 3)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 2)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
                                    | otherwise = (r, board, childs)

lets a b    | a == 1 = 2
            | a == 1 &&  b == 2 = 1
            |otherwise =3