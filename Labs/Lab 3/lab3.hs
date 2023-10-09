{- Group members
    Yuvraj Gupta
    Zachary Funk
    David Semke-}


import Data.Char(ord)

-- gives the index in the range [0,99]
hashFunc :: Int -> Int
hashFunc = mod 99

convertToInt :: String -> Int
convertToInt str =
     let convertToInt = scanl (\_ x -> ord x) 0 str
     in sum convertToInt - 10000

data HashTable a = HashTable [[a]] deriving (Show)

-- implemented by David
-- insert string into HashTable; if string is duplicate or index is not found, return HashTable unchanged
-- collisions are kept in lists at their corresponding index
insertStr :: String -> Int -> HashTable String -> HashTable String
insertStr _ _ (HashTable []) = HashTable []
insertStr str currIndex (HashTable (x:xs))
    | currIndex == putIndex = if (str `elem` x)
                                then (HashTable (x:xs))
                                else (HashTable ((str:x):xs))
    | otherwise = (HashTable (x:(getHashTable $ insertStr str (currIndex+1) (HashTable xs))))
    where putIndex = hashFunc $ convertToInt str

getHashTable :: HashTable String -> [[String]]
getHashTable (HashTable xs) = xs

hMap = HashTable $replicate 100 []

{- Implementations 

My Implementation (Yuvraj's)
First I convert the string to Int by converting each character to ASCII value and then taking their sum and 
subtracting 10000 ; just for fun
Then the integer is feeded to hashFunc which will give the index in the range [0,99] using the mod function.
Since we were working in groups, the only difference would be in the converting the string to an Int in all
of the 3 scripts.
For collisions, let us suppose that I enter 10 strings, out of which 3 strings have the same Integer value,
when they are passed to hash function, they will be bound to same index. so collison would be 0.3.

David's Implementation (copy pasted from David's file)
The hash function takes an int and contrains it to range [0, 99] using the modulo operator.
The aid to the hash function, asciiNum, produces an int from a string,
where the int is produced by
1) Getting a list of all ASCII values of characters in the string - let this be ASCII_List.
2) Multiplying the first int in ASCII_List by 1, the second int by 2, the third int by 3, ...
3) Summing the resulting list of ints.

Zach's Implementation
He is converting string into integers using folds. In this, he is scanning each character and replacing it
with the sum of accumulator value and the char's corresponding ASCII value. The hash function is same and hashTable
structure is the same as the described above in the implementations.-}

