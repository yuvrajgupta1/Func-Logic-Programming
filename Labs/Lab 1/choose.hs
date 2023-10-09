
import Control.Monad(filterM)

nums :: [Int]
nums = [0, 1, 2, 3, 6]

choose :: [Int] -> Int -> [[Int]]
choose xs n =
 filter (\ys -> (length ys) == n) (filterM (\x -> [True, False]) xs)
