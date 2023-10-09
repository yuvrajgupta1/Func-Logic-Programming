
import Data.Maybe

type State = String
type Input = Char

type Transition = ( State, Input, State )

data Dfa = Dfa { delta :: [Transition] } deriving (Show)

f = ["f"]

getThird :: Maybe (a, b, c) -> Maybe c
getThird Nothing = Nothing
getThird (Just (_, _, z)) = Just z

nextState :: Dfa -> State -> Input -> Maybe State
nextState (Dfa xs) q c =
    let
        transition = 
            foldr (\(r, c'', s) acc ->
                    if ((q == r) && (c == c'')) 
                        then Just (r, c'', s) 
                        else acc
                    )
                    Nothing
                    xs
        q' = if (transition /= Nothing) then (getThird transition) else Nothing
    in q'

compute :: Dfa -> String -> Maybe State -> Maybe State
compute _ [] q = q
compute _ _ Nothing = Nothing
compute m@(Dfa xs) (a:str) (Just q) =
    let dest = compute m str (nextState m q a)
    in if (dest /= Nothing) && (fromJust dest `elem` f) then dest else Nothing

inLanguage :: Dfa -> String -> Bool
inLanguage m@(Dfa xs) str =
    (compute m str (Just "s")) /= Nothing

genAllWords :: Int -> [String]
genAllWords 0 = []
genAllWords 1 = ["a", "b"]
genAllWords n =
    [ x : y | x <- ['a', 'b'], y <- (genAllWords (n-1))]

genLanguage :: Dfa -> Int -> [String]
genLanguage _ 0 = []
genLanguage (Dfa xs) n =
    filter (inLanguage (Dfa xs)) (genAllWords n)

m = Dfa {delta = [("s",'a',"q1"),("q1",'b',"f"),("q1",'a',"f"),("f",'a',"q1")]}



-- Lab3 Starts

replaceWord :: [Char] -> [Char]
replaceWord y = foldl (\acc x -> if acc == "a" then acc ++ "bark " else acc ++ "meow " ) [] y

list10 = genLanguage m 10



--Write an expression using a combination of Monoids and mapping applied to the set of
--strings that belong to the given DFA of length 10, so that each word is converted to a
--String with “bark ” and “meow “ substrings. The result should be a list of strings again