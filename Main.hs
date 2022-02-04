module Main where
import Simulation

main :: IO ()
main = do
    putStrLn "Please enter type(1- > for dumbRobot, 2 -> for SmartRobot): "
    typeInput <- getLine
    let type = (read typeInput :: Int)

    putStrLn "Please enter amount of maxRounds: "
    maxRoundsInput <- getLine
    let maxRounds = (read maxRoundsInput :: Int)

    putStrLn "Please enter amount of obstacles: "
    obstacleInput <- getLine
    let obstacles = (read obstacleInput :: Int)

    putStrLn "Please enter amount of dirts: "
    dirtsInput <- getLine
    let dirts = (read dirtsInput :: Int)

    putStrLn "Please enter amount of robots: "
    robotsInput <- getLine
    let robots = (read robotsInput :: Int)

    putStrLn "Please enter amount of kids: "
    kidsInput <- getLine
    let kids = (read kidsInput :: Int)

    putStrLn "Please enter amount of rows: "
    rowsInput <- getLine
    let rows = (read rowsInput :: Int)

    putStrLn "Please enter amount of cols: "
    colsInput <- getLine
    let cols = (read colsInput :: Int)
    simulate type t maxRounds obstacles dirts robots kids seed rows cols