module SmartRobot where
import Utils
findTarget _ [] _ = ((-1, -1), -1)
findTarget (x, y) (t:ts) i  | x1 == x && y1 == y = (t, i)
                            | otherwise = findTarget (x, y) ts (i+1)
                    where (x1, y1) = t
smartCleanDirt r targets = 
    let (x, y, state, target) = r
        (t, index) = findTarget target targets 0
        newTargets = deleteElement targets index
    in ((x, y, 0, (-1, -1)), newTargets)


moveSmartRobot [] board robots childs targets = (board, robots, childs, targets)
moveSmartRobot (r:rs) board robots childs targets   | state == 2 = --arriba de una suciedad
    let (newRobot, newTargets) = smartCleanDirt r targets
        newRobots = tail robots ++ [newRobot]
    in moveSmartRobot rs board newRobots childs  newTargets
                                                    | state == 1 =
    let (x, y, state, (targetx, targety)) = r
    in (board, robots, childs, targets)
                                                    | otherwise = (board, robots, childs, targets)
                                                    where (_, _, state, _) = r

moveWithChild r board path childs c targets | length path > 2 =
    let (x, y, state, target) = r
        (newx, newy) = path !! 1
        midBoard = changePlace board newx newy 5
    in if c 
        then
            let newBoard = changePlace midBoard x y 3
                newRobot = (newx, newy, state, target)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 1)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
        else
            let newBoard = changePlace midBoard x y 0
                newRobot = (newx, newy, state, target)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 1)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
                                    | length path == 2 =
    let (x, y, state, target) = r
        (newx, newy) = path !! 1
        midBoard = changePlace board newx newy 5 
    in if c 
        then
            let newBoard = changePlace midBoard x y 3
                newRobot = (newx, newy, 3, target)
                -- newChilds = removeXY x y childs
                ((childx, childy, childState), index) =  find x y childs 0
                newChild = (newx, newy, 2)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
        else
            let newBoard = changePlace midBoard x y 0
                newRobot = (newx, newy, 3, target)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 2)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
                                    | length path == 1 =
    let (x, y, state, target) = r
        (newx, newy) = head path
        midBoard = changePlace board newx newy 5
    in if c 
        then
            let newBoard = changePlace midBoard x y 3
                newRobot = (newx, newy, 3, target)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 2)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
        else
            let newBoard = changePlace midBoard x y 0
                newRobot = (newx, newy, 3, target)
                ((childx, childy, childState), index) = find x y childs 0
                newChild = (newx, newy, 2)
                newChilds = replaceNTH0 childs index newChild
            in (newRobot, newBoard, newChilds)
                                    | otherwise = (r, board, childs)