module Main where
import Board
import DumbRobot

main :: IO ()
main = game --putStrLn "Hello, Haskell!"

game = 
    let board = makeBoard 3 4 2 2 123434 10 10
        robots = [(0, 9, 0), (7, 2, 0)]
        childs = [(1, 3, 0), (7, 6, 0)]
        (newBoard, newRobots, newChilds) = moveDumbRobot robots board robots childs
    in printBoard (boardToString newBoard)


game1 = 
    let board = makeBoard 3 4 2 2 123434 10 10
    in printBoard (boardToString board)