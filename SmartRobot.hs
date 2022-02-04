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
        (x', y') = target
    in if x/=x' || y /= y'
        then ((x, y, 0, target), targets)
        else
            let (t, index) = findTarget (x', y') targets 0
                newTargets = deleteElement targets index
            in ((x, y, 0, (-1,-1)), newTargets)

fixChildTarget ((x', y', (x'', y'')), i) (x, y) targets = 
    replaceNTH0 targets i (x, y, (x'', y''))

fixChildTargeted [] _ targets = targets 
fixChildTargeted pastChilds presentChilds targets = 
    let (x, y, _) = head pastChilds
        (x', y', _) = head presentChilds
        (t, i) = findTarget (x, y) targets 0
    in if i /= -1 && (x /= x' || y /= y') 
        then fixChildTargeted (tail pastChilds) (tail presentChilds) (fixChildTarget (t, i) (x', y') targets)
        else fixChildTargeted (tail pastChilds) (tail presentChilds) targets

fixRobotTarget ((x', y', (x'', y'')), i) (x, y) targets = 
    replaceNTH0 targets i (x', y', (x, y))

fixRobotTargeted [] _ targets = targets 
fixRobotTargeted pastR presentR targets = 
    let (x, y, _, _) = head pastR
        (x', y', _, _) = head presentR
        (t, i) = findTargetByR (x, y) targets 0
    in if i /= -1 && (x /= x' || y /= y') 
        then fixRobotTargeted (tail pastR) (tail presentR) (fixRobotTarget (t, i) (x', y') targets)
        else fixRobotTargeted (tail pastR) (tail presentR) targets

moveSmartRobot [] board robots childs targets = (board, robots, childs, targets)
moveSmartRobot (r:rs) board robots childs targets   | state' == 2 = --arriba de una suciedad
    let (newRobot, newTargets) = smartCleanDirt r targets
        newRobots = tail robots ++ [newRobot]
    in moveSmartRobot rs board newRobots childs  newTargets
                                                    | state' == 6 = -- cargando un niño sobre una corral sin dejarlo
    let (x, y, s, target) = r
        changedBoard = changePlace board x y 0
        (path, c) = bfsToCorral (x, y) changedBoard
    in if not (null path)
        then
            let midTargets = if notTargeted (last path) targets then (x, y, last path):targets else targets
                (newRobot, newBoard, newChilds, newTargets) =  moveWithChild (x, y, s, last path) board path childs True midTargets False
                newRobots = tail robots ++ [newRobot]
            in moveSmartRobot rs newBoard newRobots newChilds newTargets
        else
            let (tPath, midTargets) = bfsSmartRobot (x, y) board targets
                (newBoard, newRobot, newChilds, newTargets) = moveRobot (x, y, s, last tPath) board tPath childs midTargets
                newRobots = tail robots ++ [newRobot]
            in moveSmartRobot rs newBoard newRobots newChilds newTargets
                                                    | state' == 5 = -- cargando un niño sobre una suciedad
    let (x, y, s, target) = r
        changedBoard = changePlace board x y 0
        (path, c) = bfsToCorral (x, y) changedBoard
    in if not (null path)
        then
            let midTargets = if notTargeted (last path) targets then (x, y, last path):targets else targets
                (newRobot, newBoard, newChilds, newTargets) =  moveWithChild (x, y, s, last path) board path childs c midTargets True
                newRobots = tail robots ++ [newRobot]
            in moveSmartRobot rs newBoard newRobots newChilds newTargets
        else
            let (tPath, midTargets) = bfsSmartRobot (x, y) board targets
                (newBoard, newRobot, newChilds, newTargets) = moveRobot (x, y, s, last tPath) board tPath childs midTargets
                newRobots = tail robots ++ [newRobot]
            in moveSmartRobot rs newBoard newRobots newChilds newTargets
                                                    | state' == 1 = -- cargando un niño
    let (x, y, s, target) = r
        changedBoard = changePlace board x y 0
        (path, c) = bfsToCorral (x, y) changedBoard
    in if not (null path)
        then
            let midTargets = if notTargeted (last path) targets then (x, y, last path):targets else targets
                (newRobot, newBoard, newChilds, newTargets) =  moveWithChild (x, y, s, last path) board path childs c midTargets False
                newRobots = tail robots ++ [newRobot]
            in moveSmartRobot rs newBoard newRobots newChilds newTargets
        else
            let (tPath, midTargets) = bfsSmartRobot (x, y) board targets
                (newBoard, newRobot, newChilds, newTargets) = moveRobot (x, y, s, last tPath) board tPath childs midTargets
                newRobots = tail robots ++ [newRobot]
            in moveSmartRobot rs newBoard newRobots newChilds newTargets
                                                    | otherwise =
    let (x, y, state, target) = r

        (path, midTargets) = bfsSmartRobot (x, y) board targets
        (newBoard, newRobot, newChilds, newTargets) = moveRobot (x, y, state, last path) board path childs midTargets
        newRobots = tail robots ++ [newRobot]
    in moveSmartRobot rs newBoard newRobots newChilds newTargets
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
        let newRobot = (x', y', 1, (-1, -1))
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

moveWithChild r board path childs c targets d| length path > 2 =
    let (x, y, state, target) = r
        (newx, newy) = path !! 1
        newTarget = tail (tail path)
        midBoard = changePlace board newx newy 5
        (_, i) = find x y childs 0
        newChilds = replaceNTH0 childs i (newx, newy, 1)
    in if c
        then ((newx, newy, 1, last newTarget), changePlace midBoard x y 3, newChilds, targets)
        else 
            if d
            then
                if indexBoard newx newy board == 2 
                then
                    ((newx, newy, 5, last newTarget), changePlace midBoard x y 2, newChilds, targets)
                else
                    if indexBoard newx newy board == 3
                    then 
                        ((newx, newy, 6, last newTarget), changePlace midBoard x y 2, newChilds, targets)
                    else
                        ((newx, newy, 1, last newTarget), changePlace midBoard x y 2, newChilds, targets)
            else 
                if indexBoard newx newy board == 2 
                then
                    ((newx, newy, 5, last newTarget), changePlace midBoard x y 0, newChilds, targets)
                else
                    if indexBoard newx newy board == 3
                    then 
                        ((newx, newy, 6, last newTarget), changePlace midBoard x y 0, newChilds, targets)
                    else
                        ((newx, newy, 1, last newTarget), changePlace midBoard x y 0, newChilds, targets)
                                | length path == 2 =
    let (x, y, _, target) = r
        (newx, newy) = path !! 1
        midBoard = changePlace board newx newy 5
        (_, i) = findTarget (newx, newy) targets 0
        newTargets = deleteElement targets i
        (_, i1) =  find x y childs 0
        newChilds = replaceNTH0 childs i1 (newx, newy, 2)
    in if c
        then ((newx, newy, 3, (-1,-1)), changePlace midBoard x y 3, newChilds, newTargets)
        else ((newx, newy, 3, (-1,-1)), changePlace midBoard x y 0, newChilds, newTargets)
                                    | length path == 1 =
    let (x, y, _, target) = r
        (newx, newy) = head path
        midBoard = changePlace board newx newy 5
        (_, i) = findTarget (newx, newy) targets 0
        newTargets = deleteElement targets i
        (_, i1) =  find x y childs 0
        newChilds = replaceNTH0 childs i1 (newx, newy, 2)
    in if c
        then ((newx, newy, 3, (-1,-1)), changePlace midBoard x y 3, newChilds, newTargets)
        else ((newx, newy, 3, (-1,-1)), changePlace midBoard x y 0, newChilds, newTargets)
                                    | otherwise = (r, board, childs, targets)

spotNewPosition robot board (newx, newy) oldSpot childs targets
    | spot == 2 =
    -- camina hacia una suciedad
        let (oldx, oldy, state, target) = robot
            -- parte en que se modifica el tablero
            inBetweenBoard = changePlace board oldx oldy oldSpot
            newBoard = changePlace inBetweenBoard newx newy 5
            -- se inicializa el nuevo robot con estado 2 y target una lista vacia porque llego a su objetivo
            newRobot = (newx, newy, 2, (-1,-1))
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
            newRobot = (newx, newy, 1, (-1,-1))
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
            newRobot = (newx, newy, 4, (-1,-1))
        in  (newRobot, newBoard, childs, targets)
    | otherwise =
    -- camina normalmente
        let (oldx, oldy, state, target) = robot
        in if state == 3 || state == 4
            then
                let
                    inBetweenBoard = changePlace board oldx oldy oldSpot
                    newBoard = changePlace inBetweenBoard newx newy 5
                    newRobot = (newx, newy, 0, (-1,-1))
                in (newRobot, newBoard, childs, targets)
            else
                let
                    inBetweenBoard = changePlace board oldx oldy oldSpot
                    newBoard = changePlace inBetweenBoard newx newy 5
                    newRobot = (newx, newy, state, (-1,-1))
                in (newRobot, newBoard, childs, targets)
    where   spot = indexBoard newx newy board
            (_, _, state, _) = robot

