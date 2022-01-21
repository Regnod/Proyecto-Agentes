module Robot where
import Utils
import MyRandom
import Recorridos

-- states
-- free -> 0
-- holding baby -> 1
-- over a dirt -> 2
-- leaving child -> 3
-- over a corral ->4


-- tengo que tener en cuenta:
-- cuando llego a un corral con un niño
-- cuando llego a una suciedad
-- cuando llego a un niño

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
    -- TODO: verificar que cuando deja al niño cese el movimiento 
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

moveRobot robot board path longitude childs | longitude > 0 && not (null path) && state == 3 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 6 childs
    in (newBoard, newRobot, newChilds)
                                        | longitude > 0 && not (null path) && state == 4 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 3 childs
    in moveRobot newRobot newBoard (tail path) (longitude-1) newChilds
                                        | longitude > 0 && not (null path) && state == 0 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 0 childs
    in moveRobot newRobot newBoard (tail path) (longitude-1) newChilds
                                        | longitude > 0 && not (null path) && state == 1 =
    let (oldx, oldy, state) = robot
        (newx, newy) = head path
        (newRobot, newBoard, newChilds)= spotNewPosition robot board (head path) 0 childs
    in moveRobot newRobot newBoard (tail path) (longitude-1) newChilds
                                        | otherwise = (board, robot, childs)
                                    where (oldx, oldy, state) = robot

-- este metodo mueve un solo robot y devuelve el board y el robot luego de realizar el movimiento
moveDumbRobot [] board  robots childs = (board, robots, childs)
moveDumbRobot (r:rs) board  robots childs   | state == 0 = -- significa que no carga un niño
    -- ver a donde me muevo y moverme
    let (x, y, _) = r
        tracker = buildBoard 5 5 (-1,-1)
        nearestPath = bfsDumbRobot [(x,y)] board [] tracker
        (newBoard, newRobot, newChilds) = moveRobot r board nearestPath 1 childs
        newRobots = tail robots ++ [newRobot]
    in moveDumbRobot rs newBoard newRobots newChilds
                                    | state == 1 = -- significa que carga un niño
    let (x, y, _) = r
        tracker = buildBoard 5 5 (-1,-1)
        nearestPathToCorral = bfsCorral [(x,y)] board [] 3 tracker
        (newBoard, newRobot, newChilds) = moveRobot r board nearestPathToCorral 2 childs
        newRobots = tail robots ++ [newRobot]
    in moveDumbRobot rs newBoard newRobots newChilds
                                    | otherwise = -- significa que está sobre una suciedad
    let (x, y, _) = r
        newRobot = (x, y, 0)
        newRobots = tail robots ++ [newRobot]
    in moveDumbRobot rs board newRobots childs
                                    where (_, _, state) = r



    -- let tracker = buildBoard 5 5 (-1,-1)
    --     (x, y, state) = r
    --     nearestPath = bfsDumbRobot [(x,y)] board [] tracker
    -- in  -- aqui va el if del estado
    --     if state == 1
    --         then 
    --             -- only can move one time
    --             let tracker = buildBoard 5 5 (-1,-1)
    --                 nearestPathCorral = bfsCorral [(x,y)] board [] 3 tracker

    --         else 1












-- module Robot where
-- import Utils
-- import MyRandom
-- import Recorridos

-- states
-- free -> 0
-- holding baby -> 1
-- over a dirt -> 2

-- -- move robot carrying a child
-- moveRobot robot board path longitude | longitude > 0 && not (null path) =
--     let (oldx, oldy, state) = robot
--     in  if state == 2
--         then
--             let (newx, newy) = head path
--                 newRobot = (newx, newy, state)
--                 col = board !! oldx
--                 newCol = replaceNTH0 col oldy 2
--                 inBetweenBoard = replaceNTH0 board oldx newCol

--                 col = inBetweenBoard !! newx
--                 newCol = replaceNTH0 col newy 5
--                 newBoard = replaceNTH0 inBetweenBoard newx newCol
--             in moveRobot newRobot newBoard (tail path) (longitude-1)
--         else
--             let
--                 (newx, newy) = head path
--                 newRobot = (newx, newy, state)
--                 col = board !! oldx
--                 newCol = replaceNTH0 col oldy 0
--                 inBetweenBoard = replaceNTH0 board oldx newCol

--                 col = inBetweenBoard !! newx
--                 newCol = replaceNTH0 col newy 5
--                 newBoard = replaceNTH0 inBetweenBoard newx newCol
--             in moveRobot newRobot newBoard (tail path) (longitude-1)
--                                         | otherwise = (board, robot)

-- moveRobot robot board path longitude| longitude > 0 && not (null path) && state == 2 =
--     let (oldx, oldy, state) = robot
--         (newx, newy) = head path
--         col = board !! oldx
--         newCol = replaceNTH0 col oldy 2
--         inBetweenBoard = replaceNTH0 board oldx newCol
--         -- TODO: tener en cuenta cuando el paso al q llega es un bebe
--         col1 = inBetweenBoard !! newx
--         newCol1 = replaceNTH0 col newy 5
--         newBoard = replaceNTH0 inBetweenBoard newx newCol
--         newRobot = (newx, newy, 0)
--     in moveRobot newRobot newBoard (tail path) (longitude-1)
--                                     | otherwise = (board, robot)
--                                     where (oldx, oldy, state) = robot

-- -- este metodo mueve un solo robot y devuelve el board y el robot luego de realizar el movimiento
-- moveDumbRobot [] board  robots = (board, robots)
-- moveDumbRobot (r:rs) board  robots  | state == 0 = -- significa que no carga un niño
--     -- ver a donde me muevo y moverme
--     let (x, y, _) = r
--         tracker = buildBoard 5 5 (-1,-1)
--         nearestPath = bfsDumbRobot [(x,y)] board [] tracker
--         (newBoard, newRobot) = moveRobot r board nearestPath 1
--         newRobots = tail robots ++ [newRobot]
--     in moveDumbRobot rs newBoard newRobots
--                                     | state == 1 = -- significa que carga un niño
--     let (x, y, _) = r
--         tracker = buildBoard 5 5 (-1,-1)
--         nearestPathToCorral = bfsCorral [(x,y)] board [] 3 tracker
--         (newBoard, newRobot) = moveRobot r board nearestPathToCorral 2
--         newRobots = tail robots ++ [newRobot]
--     in moveDumbRobot rs newBoard newRobots
--                                     | otherwise = -- significa que está sobre una suciedad
--     let (x, y, _) = r
--         newRobot = (x, y, 0)
--         newRobots = tail robots ++ [newRobot]
--     in moveDumbRobot rs board newRobots
--                                     where (_, _, state) = r



--     -- let tracker = buildBoard 5 5 (-1,-1)
--     --     (x, y, state) = r
--     --     nearestPath = bfsDumbRobot [(x,y)] board [] tracker
--     -- in  -- aqui va el if del estado
--     --     if state == 1
--     --         then 
--     --             -- only can move one time
--     --             let tracker = buildBoard 5 5 (-1,-1)
--     --                 nearestPathCorral = bfsCorral [(x,y)] board [] 3 tracker

--     --         else 1