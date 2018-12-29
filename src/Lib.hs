module Lib
    (
    run, buildTree, rank, select
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type WaveletTree = ()

run :: String
run = undefined


buildTree :: String -> WaveletTree
buildTree = undefined

-- rank tree (select tree j char) char === j
-- returns the number of chars at or before idx
rank :: WaveletTree -> Int -> Char -> Int
rank tree idx char = undefined

-- returns the index of the "occNumber"th occurance of the char
select :: WaveletTree -> Int -> Char -> Int
select tree occNumber char = undefined
