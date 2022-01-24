module Main where
import Board
import DumbRobot
import MyRandom

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

game robots board childs t newSeed =
    let t1 = t-1
    in if checkHouse board
        then "ended"
        else game robots board childs t newSeed


myCycle t robots board childs =
    let (newBoard, newRobots, newChilds) = moveDumbRobot robots board robots childs
    in myCycle (t-1) newRobots newBoard newChilds