module Lib

where

-- string  = "abcdbcaddbca"

import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector -- todo: swap for succinct vector
import Data.List.Split

import qualified Data.Vector.Storable as S

import Debug.Trace

import Data.Word (Word64)

import qualified HaskellWorks.Data.RankSelect.Base as RS
import qualified HaskellWorks.Data.RankSelect.CsPoppy as CP

string = "abcdbcaddbca"

{-

t = buildTree string
λ: select t 3 'a'
11
λ: string
"abcdbcaddbca"
λ: select t 2 'a'
6
λ: select t 1 'a'
0
λ: select t 1 'b'
1
λ: select t 1 'c'
2

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


toWord64 :: [Bool] -> [Word64]
toWord64 x = map getInt $ chunksOf 64 x
  where integerize (pow, acc) b = (pow+1 , if b then acc+(2^pow) else acc)
        getInt = snd . foldl' integerize (0, 0)



data WTree = Leaf Char | Tree { left :: WTree
                             , right :: WTree
                             , index :: CP.CsPoppy--S.Vector Word64
                             , alphabet :: Map.Map Char Bool
                             } deriving (Show) --,Eq)

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
        index = CP.makeCsPoppy $ S.fromList $ toWord64 $ map (alphabet Map.!) string
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
rank :: WTree -> Word64 -> Char -> Word64
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
rankI :: Bool -> CP.CsPoppy -> Word64 -> Word64
rankI False index i = RS.rank0 index i
rankI True  index i = RS.rank1 index i


-- selectI False index (rankI False index j) === j
-- The ith True/False in the bitvector
-- TODO replace with succinct bitvector implementation
selectI :: Bool -> CP.CsPoppy -> Word64 -> Word64
selectI False index i = RS.select0 index i
selectI True  index i = RS.select1 index i


constructPath' :: WTree -> Char -> [(Bool, CP.CsPoppy)] -> [(Bool, CP.CsPoppy)]
constructPath' (Leaf c) char acc | char /= c = error "constructPath: wrong way!"
constructPath' (Leaf c) char acc | char == c = acc
constructPath' (Tree l r i a) c acc = constructPath' next c acc'
  where
    direction = a Map.! c
    next = if direction then r else l
    acc' = (direction, i):acc

constructPath :: WTree -> Char -> [(Bool, CP.CsPoppy)]
constructPath t c = constructPath' t c []

selectRec :: Word64 -> [(Bool, CP.CsPoppy)] -> Word64
selectRec i [] = max 0 $ i - 1 -- correct 1-based indexing to be 0-based
selectRec i ((dir, vec):xs) = selectRec (selectI dir vec i) xs

-- returns the index of the "occNumber"th occurrence of the char
select :: WTree -> Word64 -> Char -> Word64
select tree occNumber char = selectRec occNumber (constructPath tree char)
