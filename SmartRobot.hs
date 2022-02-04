module SmartRobot where
import Utils
import Recorridos
import Debug.Trace
findTarget _ [] _ = ((-1, -1, (-1,-1)), -1)
findTarget (x, y) (t:ts) i  | x1 == x && y1 == y = (t, i)
                            | otherwise = findTarget (x, y) ts (i+1)
                    where (x1, y1, _) = t
smartCleanDirt r targets = 
    let (x, y, state, target) = r
        (t, index) = findTarget (x, y) targets 0
        newTargets = deleteElement targets index
    in ((x, y, 0, []), newTargets)


moveSmartRobot [] board robots childs targets = (board, robots, childs, targets)
moveSmartRobot (r:rs) board robots childs targets   | state' == 2 = --arriba de una suciedad
    let (newRobot, newTargets) = smartCleanDirt r targets
        newRobots = tail robots ++ [newRobot]
    in moveSmartRobot rs board newRobots childs  newTargets
                                                    | state' == 1 = -- cargando un niño
    let (x, y, s, target) = r
        changedBoard = changePlace board x y 0
        (path, c) = bfsToCorral (x, y) changedBoard
    in if not (null path)
        then
            let midTargets = if notTargeted (x, y) targets then (x, y, (x, y)):targets else targets
                (newRobot, newBoard, newChilds, newTargets) =  moveWithChild (x, y, s, path) board path childs c midTargets
                newRobots = tail robots ++ [newRobot]
            in moveSmartRobot rs newBoard newRobots newChilds  newTargets
        else
            let (tPath, midTargets) = if not (null target) && targeted (last target) targets
                                    then (target, targets)
                                    else bfsSmartRobot (x, y) board targets
                (newBoard, newRobot, newChilds, newTargets) = moveRobot (x, y, s, tPath) board tPath childs midTargets
                newRobots = tail robots ++ [newRobot]
            in moveSmartRobot rs newBoard newRobots newChilds  newTargets
                                                    | otherwise = 
    let (x, y, state, target) = r
        
        (path, midTargets) = if not (null target) && targeted (last target) targets
                                    then (target, targets)
                                    else bfsSmartRobot (x, y) board targets
        (newBoard, newRobot, newChilds, newTargets) = moveRobot (x, y, state, path) board path childs midTargets
        newRobots = tail robots ++ [newRobot]
    in moveSmartRobot rs newBoard newRobots newChilds  newTargets
                                                    where (_, _, state', _) = r

moveRobot (x', y', state', target') board path childs targets
    | not(null path) && state' == 3 = 
    --
        let (newx, newy) = head path
            (newRobot, newBoard, newChilds, newTargets) = spotNewPosition (x', y', state', target') board (head path) 6 childs targets
        in  (newBoard, newRobot, newChilds, newTargets)
    | not(null path) && state' == 4 = 
    --
        let (newx, newy) = head path
            (newRobot, newBoard, newChilds, newTargets) = spotNewPosition (x', y', state', target') board (head path) 3 childs targets
        in  (newBoard, newRobot, newChilds, newTargets)
    | not(null path) && state' == 5 = 
    --
        let newRobot = (x', y', 1, [])
            -- se elimina ese target de la lista de targets
            (_, i) = findTarget (x', y') targets 0
            newTargets = deleteElement targets i
        in (board, newRobot, childs, newTargets)
    | not(null path) && state' == 0 =
    --
        let (newx, newy) = head path
            (newRobot, newBoard, newChilds, newTargets) = spotNewPosition (x', y', state', target') board (head path) 0 childs targets
        in  (newBoard, newRobot, newChilds, newTargets)
    | otherwise = (board, (x', y', state', target'), childs, targets)

moveWithChild r board path childs c targets | length path > 2 =
    let (x, y, state, target) = r
        (newx, newy) = path !! 1
        newTarget = tail (tail path)
        midBoard = changePlace board newx newy 5
        (_, i) = find x y childs 0
        newChilds = replaceNTH0 childs i (newx, newy, 1)
    in if c 
        then ((newx, newy, state, newTarget), changePlace midBoard x y 3, newChilds, targets)
        else ((newx, newy, state, newTarget), changePlace midBoard x y 0, newChilds, targets)
                                    | length path == 2 =
    let (x, y, _, target) = r
        (newx, newy) = path !! 1
        midBoard = changePlace board newx newy 5
        (_, i) = findTarget (last target) targets 0
        newTargets = deleteElement targets i
        (_, i1) =  find x y childs 0
        newChilds = replaceNTH0 childs i1 (newx, newy, 2)
    in if c 
        then ((newx, newy, 3, []), changePlace midBoard x y 3, newChilds, newTargets)
        else ((newx, newy, 3, []), changePlace midBoard x y 0, newChilds, newTargets)
                                    | length path == 1 =
    let (x, y, _, target) = r
        (newx, newy) = head path
        midBoard = changePlace board newx newy 5
        (_, i) = findTarget (last target) targets 0
        newTargets = deleteElement targets i
        (_, i1) =  find x y childs 0
        newChilds = replaceNTH0 childs i1 (newx, newy, 2)
    in if c 
        then ((newx, newy, 3, []), changePlace midBoard x y 3, newChilds, newTargets)
        else ((newx, newy, 3, []), changePlace midBoard x y 0, newChilds, newTargets)
                                    | otherwise = (r, board, childs, targets)

spotNewPosition robot board (newx, newy) oldSpot childs targets 
    | spot == 2 = 
    -- camina hacia una suciedad
        let (oldx, oldy, state, target) = robot
            -- parte en que se modifica el tablero
            inBetweenBoard = changePlace board oldx oldy oldSpot
            newBoard = changePlace inBetweenBoard newx newy 5
            -- se inicializa el nuevo robot con estado 2 y target una lista vacia porque llego a su objetivo
            newRobot = (newx, newy, 2, [])
            -- se elimina ese target de la lista de targets
            (_, i) = findTarget (newx, newy) targets 0
            newTargets = deleteElement targets i
        in  (newRobot, newBoard, childs, newTargets)
    | spot == 4 = 
    -- camina hacia un niño
        let (oldx, oldy, state, target) = robot
            -- parte en que se modifica el tablero
            inBetweenBoard = changePlace board oldx oldy oldSpot
            newBoard = changePlace inBetweenBoard newx newy 5
            -- se inicializa el nuevo robot con estado 2 y target una lista vacia porque llego a su objetivo
            newRobot = (newx, newy, 1, [])
            -- se elimina ese target de la lista de targets
            (_, i) = findTarget (newx, newy) targets 0
            newTargets = deleteElement targets i
            -- cambiar el estado del niño a capturado por un robot y reemplazarlo en la lista de niños
            (_, index) = find newx newy childs 0
            newChild = (newx, newy, 1)
            newChilds = replaceNTH0 childs index newChild
        in  (newRobot, newBoard, newChilds, newTargets)
    | spot == 3 = 
    -- camina hacia una casilla corral 
        let (oldx, oldy, state, target) = robot
            -- parte en que se modifica el tablero
            inBetweenBoard = changePlace board oldx oldy oldSpot
            newBoard = changePlace inBetweenBoard newx newy 5
            -- se inicializa el nuevo robot con estado 2 y target una lista vacia porque llego a su objetivo
            newRobot = (newx, newy, 4, tail target)
        in  (newRobot, newBoard, childs, targets)
    | otherwise = 
    -- camina normalmente
        let (oldx, oldy, state, target) = robot
        in if state == 3 || state == 4
            then
                let 
                    inBetweenBoard = changePlace board oldx oldy oldSpot
                    newBoard = changePlace inBetweenBoard newx newy 5
                    newRobot = (newx, newy, 0, tail target)
                in (newRobot, newBoard, childs, targets)
            else
                let 
                    inBetweenBoard = changePlace board oldx oldy oldSpot
                    newBoard = changePlace inBetweenBoard newx newy 5
                    newRobot = (newx, newy, state, tail target)
                in (newRobot, newBoard, childs, targets)
    where   spot = indexBoard newx newy board
            (_, _, state, _) = robot

