-- Author: Yuvraj Gupta
-- Assignment 1: Merge Sort for Pairs.


-- this function compares 2 pairs and returns true if Pair 1 < Pair 2

comparePairs :: (Ord a) => (a,a) -> (a,a) -> Bool
comparePairs (x,y) (a,b) = (x,y) < (a,b)

-- this function merges 2 lists

merge :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) =
 if comparePairs x y
  then  x: (merge xs (y:ys))
else  y: (merge (x:xs) ys)


-- this function sorts the list in ascending order using merge sort

mergeSort :: (Ord a) => [(a,a)] -> [(a,a)]
mergeSort [] = []
mergeSort [(a,b)] = [(a,b)]
mergeSort x = merge (mergeSort (take half x)) (mergeSort (drop half x))
 where half = div (length x) 2
