module Main where

sumalista :: Num a => [a] -> a
sumalista [] = 0
sumalista (x:xs) = x + sumalista xs

fibonacci n | n == 0 = 1
            | n == 1 = 1
            | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = putStrLn "Hello, Haskell!"
