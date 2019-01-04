module Lib

where

-- string  = "abcdbcaddbca"

import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector -- todo: swap for succinct vector

import Debug.Trace

string = "abcdbcaddbca"

{-
alphabet = {a, b, c, d}
index    = {0, 0, 1, 1}
                    11
ix      = 012345678901
string  = abcdbcaddbca

index0  = 001101011010
stringl = abbaba
stringr =       cdcddc
index1l = 011010
index1r =       010110
string  = aaabbb
                cccddd

rank(7, c) ->
    level 0 = 4 1s
    level 1 = 2 0s
-}


data WTree = Leaf Char | Tree { left :: WTree
                             , right :: WTree
                             , index :: Vector.Vector Bool
                             , alphabet :: Map.Map Char Bool
                             } deriving (Show, Eq)

run :: String
run = undefined

getCharSet :: String -> Set.Set Char
getCharSet = Set.fromList . nub . sort

buildTree :: String -> WTree
buildTree "" = error "empty tree"
buildTree string
  | length charSet == 1 = Leaf $ Set.findMin charSet
  where charSet = getCharSet string
buildTree string = Tree (buildTree left) (buildTree right) index alphabet
  where alphabet = buildAlphabet $ getCharSet string
        index = Vector.fromList $ map (alphabet Map.!) string
        left = filter (\c -> not (alphabet Map.! c)) string
        right = filter (\c -> alphabet Map.! c) string

buildAlphabet :: Set.Set Char -> Map.Map Char Bool
buildAlphabet input = Map.fromList $ zip (Set.toList input) (zeros ++ ones)
  where
    len = length input
    zeros = replicate (div len 2) False
    ones = replicate (len - length zeros) True


-- rank tree (select tree j char) char === j
-- returns the number of chars at or before idx
rank :: WTree -> Int -> Char -> Int
rank (Leaf _) _ _ = error "leaves dont have rank"
rank tree i char
  | (Leaf _) <- left tree,  not direction = nextI
  | (Leaf _) <- right tree, direction     = nextI
  | not direction = rank (left tree)  nextI char
  | otherwise     = rank (right tree) nextI char
  where
        direction = alphabet tree Map.! char
        nextI = rankI direction (index tree) i

-- rankI False index (selectI False index j) === j
-- The number of True/False at or before idx
-- TODO replace with succinct bitvector implementation
rankI :: Bool -> Vector.Vector Bool -> Int -> Int
rankI b index i = length $ Vector.filter (==b) $ Vector.take (i+1) index

-- selectI False index (rankI False index j) === j
-- The ith True/False in the bitvector
-- TODO replace with succinct bitvector implementation
selectI :: Bool -> Vector.Vector Bool -> Int -> Int
selectI b index i = max 0 $ found - 1
  where
    Just found = Vector.elemIndex i counts
    counts     = Vector.scanl (\acc el -> if el == b then acc+1 else acc) 0 index

constructPath' :: WTree -> Char -> [(Bool, Vector.Vector Bool)] -> [(Bool, Vector.Vector Bool)]
constructPath' (Leaf c) char acc | char /= c = error "constructPath: wrong way!"
constructPath' (Leaf c) char acc | char == c = acc
constructPath' (Tree l r i a) c acc = constructPath' next c acc'
  where
    direction = a Map.! c
    next = if direction then r else l
    acc' = (direction, i):acc

constructPath :: WTree -> Char -> [(Bool, Vector.Vector Bool)]
constructPath t c = constructPath' t c []

selectRec :: Int -> [(Bool, Vector.Vector Bool)] -> Int
selectRec i [] = max 0 $ i - 1
selectRec i ((dir, vec):xs) = selectRec (selectI dir vec i + 1) xs

-- returns the index of the "occNumber"th occurrence of the char
select :: WTree -> Int -> Char -> Int
select tree occNumber char = selectRec occNumber (constructPath tree char)
