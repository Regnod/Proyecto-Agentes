module MyRandom where
import Control.Monad.State ( State, evalState, get, put )
import System.Random ( StdGen , mkStdGen, random, randomR, Random, randomIO)
import Data.Fixed (mod')

type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: R Int
rand = do
    gen <- get
    let (r, gen') = random gen
    put gen'
    return r

myRand :: Int -> Int
myRand seed = do 
    let r = runRandom rand seed
    if r == seed then runRandom rand (seed+1) else r

myRandom :: Int -> Int -> (Int,Int)
myRandom seed top = let nextSeed = myRand seed in (nextSeed, mod nextSeed top)
-- myRandomCapped seed top = let nextSeed = myRand seed in (nextSeed, mod nextSeed top)