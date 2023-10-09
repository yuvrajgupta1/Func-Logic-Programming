import Data.Char (ord)
import Control.Applicative

{- Group Members include 
    David Semke
    Zachary Funk
    Yuvraj Gupta
-}

-- hash function; returns int in range [0, 99]
pseudorandInt :: Int -> Int
pseudorandInt x = x `mod` 100


-- method to get integer from string
asciiNum :: String -> Int
asciiNum str = 
    let asciiNums = scanl (\_ x -> ord x) 0 str
    in sum $ (ZipList $ map (*) [1, 2..]) <*> (ZipList asciiNums)


data HashTable a = HashTable [[a]] deriving (Show)


-- insert string into HashTable; if string is duplicate or index is not found, return HashTable unchanged
-- collisions are kept in lists at their corresponding index
insertStr :: String -> Int -> HashTable String -> HashTable String
insertStr _ _ (HashTable []) = (HashTable [])
insertStr str currIndex (HashTable (x:xs))
    | currIndex == putIndex = if (str `elem` x) 
                                then (HashTable (x:xs)) 
                                else (HashTable ((str:x):xs))
    | otherwise = (HashTable (x:(getHashTable $ insertStr str (currIndex+1) (HashTable xs))))
    where putIndex = pseudorandInt $ asciiNum str


getHashTable :: HashTable String -> [[String]]
getHashTable (HashTable xs) = xs


{- Discussion

My Implementation (David's)

The hash function takes an int and contrains it to range [0, 99] using the modulo operator.
The aid to the hash function, asciiNum, produces an int from a string,
where the int is produced by
1) Getting a list of all ASCII values of characters in the string - let this be ASCII_List.
2) Multiplying the first int in ASCII_List by 1, the second int by 2, the third int by 3, ...
3) Summing the resulting list of ints.

The HashTable was simply made to be a list of lists of type [[a]].


Yuvraj's Implementation



Zachary's Implementation



-} 




















