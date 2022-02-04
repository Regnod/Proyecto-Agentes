module Simulation where
import Environment
import Child
import SmartRobot
import DumbRobot
import Debug.Trace
import Board

myCycleDumbRobot t robots board childs seed 
    | t == 0 = (board, robots, childs, seed)
    | otherwise =
    --
        let (midBoard, newRobots, midChilds) = moveDumbRobot robots board robots childs
            (newBoard, newChilds, newSeed) = childMove midChilds midBoard seed midChilds
        in trace ("\n----------------------t-cycle----------------------------\n\n"++(boardToString newBoard) ) myCycleDumbRobot (t-1) newRobots newBoard newChilds newSeed


myCycleSmartRobot t robots board childs seed targets    
    | t == 0 = (board, robots, childs, seed, targets)
    | otherwise =
    --
        let (midBoard, newRobots, midChilds, newTargets) = moveSmartRobot robots board robots childs targets
            (newBoard, newChilds, newSeed) = childMove midChilds midBoard seed midChilds
        in trace ("\n----------------------t-cycle----------------------------\n\n"++(boardToString newBoard) ) myCycleSmartRobot (t-1) newRobots newBoard newChilds newSeed newTargets

simulate type t maxRounds obstacles dirts robots kids seed rows cols 
    | type == 1 = 
    -- DumbRobot simulation
        let (robots, board, childs) = makeBoard obstacles dirts robots kids seed rows cols
            (newBoard, newRobots, newChild, newSeed, newTargets) = trace("\n----------------------start----------------------------\n\n"++boardToString board) simulationDumbRobot robots board childs maxRounds seed t
        in printBoard (boardToString newBoard)
    | type == 2 =
    -- SmartRobot simulation
        let (robots, board, childs) = makeBoard obstacles dirts robots kids seed rows cols
            midRobots = makeRobotIntel robots
            (newBoard, newRobots, newChild, newSeed) = trace("\n----------------------start----------------------------\n\n"++boardToString board) simulationDumbRobot midRobots board childs maxRounds seed t
        in printBoard (boardToString newBoard)
    | putStrLn "The type is wrong"

simulationDumbRobot robots board childs times seed t 
    | times == 0 = trace ("\n----------------------Final----------------------------\n\n")(board, robots, childs, seed)
    | otherwise =
    --
        if checkHouse board
            then (board, robots, childs, seed)
            else
                let (firstBoard, firstRobots, firstChilds, firstSeed) = myCycleDumbRobot (t-1) robots board childs seed
                    (midBoard, newRobots, newChilds, midSeed) = myCycleDumbRobot 1 firstRobots firstBoard firstChilds firstSeed
                    midChilds = getPastPossWithPresentState firstChilds newChilds
                    (newBoard, newSeed) = spawnMultipleDirt midChilds midBoard midSeed
                in trace ("\n--------------------------dirt-gen--------------------------\n\n"++boardToString newBoard) simulationDumbRobot newRobots newBoard newChilds (times-1) newSeed t

simulationSmartRobot robots board childs times seed t targets
    | times == 0 = trace ("\n----------------------Final----------------------------\n\n")(board, robots, childs, seed, targets)
    | otherwise =
    --
        if checkHouse board
            then (board, robots, childs, seed)
            else
                let (firstBoard, firstRobots, firstChilds, firstSeed, midTargets) = myCycleSmartRobot (t-1) robots board childs seed targets
                    (midBoard, newRobots, newChilds, midSeed, newTargets) = myCycleSmartRobot 1 firstRobots firstBoard firstChilds firstSeed midTargets
                    midChilds = getPastPossWithPresentState firstChilds newChilds
                    (newBoard, newSeed) = spawnMultipleDirt midChilds midBoard midSeed
                in trace ("\n--------------------------dirt-gen--------------------------\n\n"++boardToString newBoard) simulationSmartRobot newRobots newBoard newChilds (times-1) newSeed t newTargets