module Main where
import Board ( boardToString, printBoard, checkHouse, makeBoard )
import DumbRobot ( moveDumbRobot )
import MyRandom ( myRandom )
import Child ( childMove, getPastPossWithPresentState )
import Debug.Trace
import Environment
main :: IO ()
main = putStrLn (gameLoop 4 123458489 3 4 2 2 10 10)--putStrLn "Hello, Haskell!"

-- game1 = 
--     let (robots, board, childs)  = makeBoard 3 4 2 2 123434 10 10
--         -- robots = [(0, 9, 0), (7, 2, 0)]
--         -- childs = [(1, 3, 0), (7, 6, 0)]
--         (newBoard, newRobots, newChilds) = moveDumbRobot robots board robots childs
--     in printBoard (boardToString newBoard)

-- game2 = 
--     let (robots, board, childs)  = makeBoard 3 4 2 2 123434 10 10
--         -- robots = [(1, 3, 0), (7, 6, 0)]
--         -- childs = [(0, 9, 0), (7, 2, 0)]
--         (newBoard, newRobots, newChilds) = moveDumbRobot robots board robots childs
--         (newBoard1, newRobots1, newChilds1) = moveDumbRobot newRobots newBoard newRobots newChilds
--     in printBoard (boardToString newBoard1)
-- game3 = 
--     let (robots, board, childs)  = makeBoard 3 4 2 2 123434 10 10
--         -- robots = [(1, 3, 0), (7, 6, 0)]
--         -- childs = [(0, 9, 0), (7, 2, 0)]
--         (newBoard, newRobots, newChilds) = moveDumbRobot robots board robots childs
--         (newBoard1, newRobots1, newChilds1) = moveDumbRobot newRobots newBoard newRobots newChilds
--         (newBoard2, newRobots2, newChilds2) = moveDumbRobot newRobots1 newBoard1 newRobots1 newChilds1
--     in printBoard (boardToString newBoard2)

-- game = 
--     let (robots, board, childs) = makeBoard 3 4 2 2 123434 10 10
--     in printBoard (boardToString board)

--3 4 2 2 number 10 10

gameLoop t seed o s r n x y= 
    let (newSeed, number) = myRandom seed 100000000000000
        (robots, board, childs) = makeBoard o s r n number x y
    in game robots board childs t newSeed

game robots board childs t seed =
    let t1 = t-1
    in if checkHouse board
        then "ended"
        else let (newBoard, newRobots, newChilds, newSeed) = myCycle t robots board childs seed
            in game newRobots newBoard newChilds t newSeed


myCycle t robots board childs seed  | t == 0 = (board, robots, childs, seed)
                                    | otherwise =
    let (midBoard, newRobots, midChilds) = moveDumbRobot robots board robots childs
        (newBoard, newChilds, newSeed) = childMove midChilds midBoard seed midChilds
    -- let (newBoard, newChilds, newSeed) = childMove childs board seed childs
        -- (newBoard, newChilds) = (midBoard, midChilds)
        -- newRobots = robots
        -- newSeed = seed
    in myCycle (t-1) newRobots newBoard newChilds newSeed

testCycle t = 
    let seed = 545465114
        (robots, board, childs) = makeBoard 3 4 2 2 seed 10 10
        (newBoard, newRobots, newChild, newSeed) = myCycle t robots board childs seed
    in printBoard (boardToString newBoard)
simulate t times= 
    let seed = 545465114
        -- times = 2
        (robots, board, childs) = makeBoard 3 4 2 2 seed 10 10
        (newBoard, newRobots, newChild, newSeed) = simulation robots board childs times seed t
    in printBoard (boardToString newBoard)

simulation robots board childs times seed t | times == 0 = (board, robots, childs, seed)
                                            | otherwise =
    let (firstBoard, firstRobots, firstChilds, firstSeed) = myCycle (t-1) robots board childs seed
        (midBoard, newRobots, newChilds, midSeed) = myCycle 1 firstRobots firstBoard firstChilds firstSeed
        midChilds = trace (show (firstChilds++newChilds)) getPastPossWithPresentState firstChilds newChilds
        (newBoard, newSeed) = spawnMultipleDirt midChilds midBoard midSeed
    in simulation newRobots newBoard newChilds (times-1) newSeed t
    -- in if checkHouse board
    --     then (board, robots, childs, seed)
    --     else let (newBoard, newRobots, newChilds, newSeed) = myCycle t robots board childs seed
    --         in simulation newRobots newBoard newChilds (times-1) newSeed t