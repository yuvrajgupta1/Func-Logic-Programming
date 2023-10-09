{-# LANGUAGE NumDecimals #-}
import Data.Maybe
import Data.List
import qualified Data.Map as Map


names =
 [ "beta",
   "alpha",
   "beta",
   "zeta",
   "gamma",
   "lambda",
   "gamma",
   "beta",
   "zeta"
 ]

numbers =
 [ "000-0000",
   "111-1111",
   "222-2222",
   "333-3333",
   "444-4444",
   "555-5555",
   "666-6666",
   "777-7777",
   "888-8888",
   "999-9999"
 ]

phoneBook =
    Map.fromListWith
        (++)
        $ map (\(k, v) -> (k, [v])) $ zipWith (\a b -> (a, b)) names numbers

-- function to count the number of names in a phoneBook
count' :: [(a,[a])] -> Int
count' = foldl(\acc (k,v) -> acc + length v) 0

-- convert phoneBook to List
listPhoneBook = Map.toList phoneBook

-- function to check if the words end with some pattern
isIn :: (Eq a) => [a] -> [a] -> Bool
sublist `isIn` xs = any (sublist `isPrefixOf`) (tails xs)

-- filter the map
phone = Map.filterWithKey (\k _ -> isIn "eta" k) phoneBook

-- convert to List
phoneETA = Map.toList phone

-- calculate percentages
percent :: Int -> Int -> Float
percent x y = (a / b) * 100
-- convert to Float
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

-- variables to store number of items in a List
total = count' listPhoneBook
totalETA = count' phoneETA
percentage = percent totalETA total