module Environment where

import Utils
import MyRandom
import Board ( boardToString, printBoard )
import Debug.Trace

square3x3 x y = [   (x, y+1), 
                    (x+1,y+1), 
                    (x+1, y), 
                    (x+1, y-1), 
                    (x, y-1), 
                    (x-1, y-1), 
                    (x-1, y),
                    (x-1, y+1)]

valid3x3Square _ [] e n valids = (e, n, valids)
valid3x3Square board (w:ws) e n valids  | validPos r c w && indexBoard x y board == 4 =
                                    valid3x3Square board ws e (n+1) valids
                                        | validPos r c w && indexBoard x y board == 0 = 
                                    valid3x3Square board ws (e+1) n (w:valids)
                                        | otherwise = valid3x3Square board ws e n valids
                                        where   (x,y) = w
                                                r = length board
                                                c = length (head board)

howManyMax c    | c == 1 = 1
                | c == 2 = 3
                | otherwise = 6

dirt board [] _ seed = (board, seed)
dirt board positions count seed | count == 0 = (board, seed)
                                | otherwise = 
    let l = length positions
        (newSeed, pos) = myRandom seed l
        (x, y) = positions !! pos
        place = indexBoard x y board
        newBoard = changePlace board x y 2
        newPositions = deleteElement positions pos
    in dirt newBoard newPositions (count-1) newSeed

spawnDirt board x y seed = 
    let (empties, childs, square) = valid3x3Square board (square3x3 x y) 0 0 []
        fullSquare = (x,y):square
        howManyChilds = howManyMax childs
        (newSeed, dirtCount) = if howManyChilds == 1 
            then let (condition, midSeed) = fifty seed in if condition then (midSeed, 1) else (midSeed, 0)
            else myRandom seed howManyChilds 
        count = if dirtCount > empties then empties else dirtCount
    in dirt board square count newSeed

spawnMultipleDirt [] board seed = (board, seed)
spawnMultipleDirt (c:cs) board seed = 
    let (x, y, state) = c -- child
    in if state /= 1 && state /= 2
        then
            let (newBoard, newSeed) = spawnDirt board x y seed
            in spawnMultipleDirt cs newBoard newSeed
        else spawnMultipleDirt cs board seed
test = 
    let board =[[0,0,0,2,0,0,0,0,0,0],
                [0,0,0,5,0,0,0,0,0,0],
                [0,2,0,0,0,3,3,3,3,2],
                [0,0,0,0,0,0,4,3,3,0],
                [0,0,2,0,0,4,0,3,0,0],
                [0,0,0,1,0,0,0,3,1,0],
                [0,0,0,0,0,0,0,0,0,0],
                [0,0,4,0,0,0,5,0,0,0],
                [0,0,1,0,0,0,0,0,0,0],
                [0,0,3,3,0,0,0,0,0,0]]
        in printBoard (boardToString board)

test1 seed = 
    let board =[[0,0,0,2,0,0,0,0,0,0],
                [0,0,0,5,0,0,0,0,0,0],
                [0,2,0,0,0,3,3,3,3,2],
                [0,0,0,0,0,0,4,3,3,0],
                [0,0,2,0,0,4,0,3,0,0],
                [0,0,0,1,0,0,0,3,1,0],
                [0,0,0,0,0,0,0,0,0,0],
                [0,0,4,0,0,0,5,0,0,0],
                [0,0,1,0,0,0,0,0,0,0],
                [0,0,3,3,0,0,0,0,0,0]]
        childs = [(6, 2, 0), (4, 6, 0), (4,4,0)]
        -- finalChilds = [(7, 2, 0), (3, 6, 0), (4,5,0)]
        -- seed = 845222222231687
        (newBoard, newSeed) = spawnMultipleDirt childs board seed
        in printBoard (boardToString newBoard)