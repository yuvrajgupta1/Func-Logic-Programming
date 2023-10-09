--Lab 1
--Author: Yuvraj Gupta
-- Comments: I was not able to run the code in VS Studio. The following code works in the interactive mode.

--Ques 1
import Data.List

let 
elimRep :: (Eq a) => [a] -> [a] --function declaration
elimRep [] = []					--base case
elimRep [x] = [x]
elimRep (x:xs) =				--function definition
	if elem x xs				--if x is found in the remaining list
	 then elimRep xs			--call function over the remaining list
	else x:elimRep xs			--else add the element back to the list

--Ques 2
let							
union :: (Eq a) => [a] -> [a] -> [a]
union [] [] = []							--base case
union a b = elimRep(elimRep a ++ elimRep b)	--add the 2 imput lists using '++' operator

--Ques 3
let
sub :: (Eq a) => [a] -> [a] -> [a]
sub [] _ = []							--base case
sub (x:xs) y =							--function declaration
 if elem x y
  then sub xs y
 else x:sub xs y


--Ques 5
let
symdif :: (Eq a) => [a] -> [a] -> [a]
symdif [] [] = []
symdif a b = union (sub a b) (sub b a)	--symmetric difference is union of A-B and B-A

--Ques 4

let
intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] [] = []
intersect a b = sub (union a b) (symdif a b)