module Simulation where
import Environment ( spawnMultipleDirt )
import Child ( childMove, getPastPossWithPresentState )
import SmartRobot
import DumbRobot ( moveDumbRobot )
import Debug.Trace ( trace )
import Board ( boardToString, printBoard, checkHouse, makeBoard )
import Utils ( makeRobotIntel, calculateRounds )

myCycleDumbRobot t robots board childs seed
    | t == 0 = (board, robots, childs, seed)
    | otherwise =
    --
        let (midBoard, newRobots, midChilds) = moveDumbRobot robots board robots childs
            (newBoard, newChilds, newSeed) = childMove midChilds midBoard seed midChilds
        in trace ("\n----------------------t-cycle----------------------------\n\n"++boardToString newBoard ) myCycleDumbRobot (t-1) newRobots newBoard newChilds newSeed


myCycleSmartRobot t robots board childs seed targets
    | t == 0 = (board, robots, childs, seed, targets)
    | otherwise =
    --
        let (midBoard, newRobots, midChilds, firstTargets) = moveSmartRobot robots board robots childs targets
            midTargets = fixRobotTargeted robots newRobots firstTargets
            (newBoard, newChilds, newSeed) = childMove midChilds midBoard seed midChilds
            newTargets = trace(show midChilds++" "++ show newChilds)fixChildTargeted midChilds newChilds midTargets
        in trace ("\n----------------------t-cycle----------------------------\n\n"++boardToString newBoard ) myCycleSmartRobot (t-1) newRobots newBoard newChilds newSeed newTargets

simulate type_ t maxRounds obstacles dirts robot kids seed rows cols
    | type_ == 1 =
    -- DumbRobot simulation
        let (robots, board, childs) = trace"dumbRobot: " makeBoard obstacles dirts robot kids seed rows cols
            (newBoard, newRobots, newChild, newSeed, newT, newMaxRounds) = trace ("\n----------------------start----------------------------\n\n"++boardToString board) simulationDumbRobot robots board childs maxRounds seed t
        in printBoard (boardToString newBoard++"\n\n La cantidad de unidades de tiempo transcurridas es: "++show (calculateRounds maxRounds newMaxRounds t newT))
    | type_ == 2 =
    -- SmartRobot simulation
        let (robots, board, childs) = trace "smartRobot" makeBoard obstacles dirts robot kids seed rows cols
            midRobots = makeRobotIntel robots
            (newBoard, newRobots, newChild, newSeed, newTargets, newT, newMaxRounds) = trace ("\n----------------------start----------------------------\n\n"++boardToString board) simulationSmartRobot midRobots board childs maxRounds seed t []
        in printBoard (boardToString newBoard++"\n\n La cantidad de unidades de tiempo transcurridas es: "++show (calculateRounds maxRounds newMaxRounds t newT))
    | otherwise = putStrLn "The type is wrong"

simulationDumbRobot robots board childs times seed t
    | times == 0 = trace "\n----------------------Final----------------------------\n\n"(board, robots, childs, seed, t, times)
    | otherwise =
    --
        if checkHouse board
            then (board, robots, childs, seed, t, times)
            else
                let (firstBoard, firstRobots, firstChilds, firstSeed) = myCycleDumbRobot (t-1) robots board childs seed
                    (midBoard, newRobots, newChilds, midSeed) = myCycleDumbRobot 1 firstRobots firstBoard firstChilds firstSeed
                    midChilds = getPastPossWithPresentState firstChilds newChilds
                    (newBoard, newSeed) = spawnMultipleDirt midChilds midBoard midSeed
                in trace ("\n--------------------------dirt-gen--------------------------\n\n"++boardToString newBoard) simulationDumbRobot newRobots newBoard newChilds (times-1) newSeed t

simulationSmartRobot robots board childs times seed t targets
    | times == 0 = trace "\n----------------------Final----------------------------\n\n"(board, robots, childs, seed, targets, t, times)
    | otherwise =
    --
        if checkHouse board
            then (board, robots, childs, seed, targets, t, times)
            else
                let (firstBoard, firstRobots, firstChilds, firstSeed, midTargets) = myCycleSmartRobot (t-1) robots board childs seed targets
                    (midBoard, newRobots, newChilds, midSeed, newTargets) = myCycleSmartRobot 1 firstRobots firstBoard firstChilds firstSeed midTargets
                    midChilds = getPastPossWithPresentState firstChilds newChilds
                    (newBoard, newSeed) = spawnMultipleDirt midChilds midBoard midSeed
                in trace ("\n--------------------------dirt-gen--------------------------\n\n"++boardToString newBoard) simulationSmartRobot newRobots newBoard newChilds (times-1) newSeed t newTargets