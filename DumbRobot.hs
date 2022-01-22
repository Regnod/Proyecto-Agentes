module DumbRobot where
import Utils
import MyRandom
import Recorridos

-- states
-- free -> 0
-- holding baby -> 1
-- over a dirt -> 2
-- leaving child -> 3
-- over a corral ->4

-- oldSpot es lo que había antes en esa posición
spotNewPosition robot board (newx, newy) oldSpot childs | spot == 2 = -- camina hacia una suciedad
    -- no va a pasar que llegue a caminar sobre una suciedad con un niño cargado
    -- en este caso se que el estado es 0
    let (oldx, oldy, state) = robot
        col = board !! oldx
        newCol = replaceNTH0 col oldy oldSpot
        inBetweenBoard = replaceNTH0 board oldx newCol
        
        col1 = inBetweenBoard !! newx
        newCol1 = replaceNTH0 col newy 5
        newBoard = replaceNTH0 inBetweenBoard newx newCol

        newRobot = (newx, newy, 2)
    in  (newRobot, newBoard, childs)
                                                        | spot == 4 = -- camina hacia un niño
    let (oldx, oldy, state) = robot
        col = board !! oldx
        newCol = replaceNTH0 col oldy oldSpot
        inBetweenBoard = replaceNTH0 board oldx newCol
        
        col1 = inBetweenBoard !! newx
        newCol1 = replaceNTH0 col newy 5
        newBoard = replaceNTH0 inBetweenBoard newx newCol

        newRobot = (newx, newy, 1)
        ((childx, childy, childState), index) = find newx newy childs 0
        newChild = (childx, childy, 1)
        newChilds = replaceNTH0 childs index newChild
    in  (newRobot, newBoard, newChilds)
                                                        | spot == 3 && state == 1 = -- camina hacia una casilla corral 
    let (oldx, oldy, state) = robot
        newRobot = (newx, newy, 3)
        newChilds = removeXY oldx oldy childs
        -- ((childx, childy, childState), index) = find oldx oldy childs 0
        -- -- eliminar ese niño de la lista de niños
        -- newChild = (childx, childy, 2)
        -- newChilds = replaceNTH0 childs index newChild
    in  (newRobot, board, newChilds)
                                                        | otherwise = -- camina normalmente
    let (oldx, oldy, state) = robot
        col = board !! oldx
        newCol = replaceNTH0 col oldy oldSpot
        inBetweenBoard = replaceNTH0 board oldx newCol
        
        col1 = inBetweenBoard !! newx
        newCol1 = replaceNTH0 col newy 5
        newBoard = replaceNTH0 inBetweenBoard newx newCol
        newRobot = (newx, newy, 4)
    in (newRobot, newBoard, childs)
                                                        where   spot = indexBoard newx newy board
                                                                (_, _, state) = robot

cleanDirt robot = 
    let (x, y, _) = robot
        newRobot = (x,y, 0)
    in newRobot

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
    in (newBoard, newRobot, newChilds)-- moveRobot newRobot newBoard (tail path) (longitude-1) newChilds
    -- este es el caso para cuando carga el niño, solo entra aqui en caso de cargar uno
                                        | longitude > 0 && not (null path) && state == 1 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 0 childs
        (_, _, currentState) = newRobot
        -- cuando se mueve continua teniendo estado 1 o pasa a estado 3 y cesa el movimiento
    in if currentState == 3 
        then (newBoard, newRobot, newChilds) 
        else moveRobot newRobot newBoard (tail path) (longitude-1) newChilds
                                        | otherwise = (board, robot, childs)
                                    where (oldx, oldy, state) = robot

-- este metodo mueve un solo robot y devuelve el board y el robot luego de realizar el movimiento
moveDumbRobot [] board  robots childs = (board, robots, childs)
moveDumbRobot (r:rs) board  robots childs   | state == 2 =
    let newRobot = cleanDirt r
        newRobots = tail robots ++ [newRobot]
    in moveDumbRobot rs board newRobots childs
                                    | state == 1 = -- significa que carga un niño
    let (x, y, _) = r
        tracker = buildBoard 5 5 (-1,-1)
        nearestPathToCorral = bfsCorral [(x,y)] board [] 3 tracker
        (newBoard, newRobot, newChilds) = moveRobot r board nearestPathToCorral 2 childs
        newRobots = tail robots ++ [newRobot]
    in moveDumbRobot rs newBoard newRobots newChilds
                                    | otherwise = -- significa que no carga un niño
    -- ver a donde me muevo y moverme
    let (x, y, _) = r
        tracker = buildBoard 5 5 (-1,-1)
        nearestPath = bfsDumbRobot [(x,y)] board [] tracker
        (newBoard, newRobot, newChilds) = moveRobot r board nearestPath 1 childs
        newRobots = tail robots ++ [newRobot]
    in moveDumbRobot rs newBoard newRobots newChilds
                                    where (_, _, state) = r