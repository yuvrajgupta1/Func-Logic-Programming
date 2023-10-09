import Data.Char(ord)

-- new data type HashTable
newtype HashTable a = HashTable [[a]] deriving (Show)

-- getHashTable as a list of lists
getHashTable :: HashTable a -> [[a]]
getHashTable (HashTable xs) = xs

-- empty HashTable
hMap = HashTable $replicate 100 []

-- calculate index where the word will be stored
hashFunc :: Int -> Int
hashFunc x =  mod x 100

-- convert the string to its corresponding int value
convertToInt :: String -> Int
convertToInt str =
     let convertToInt = scanl (\_ x -> ord x) 0 str
     in sum convertToInt

-- stores index of the string
index' x = hashFunc $ convertToInt x

-- insert string in HashTable
insertStr :: String -> Int -> HashTable String -> HashTable String
insertStr _ _ (HashTable []) = HashTable []
insertStr str currIndex (HashTable (x:xs))
    | currIndex == putIndex = if str `elem` x
                                then HashTable (x:xs)
                                else HashTable ((str:x):xs)
    | otherwise = HashTable (x:getHashTable (insertStr str (currIndex+1) (HashTable xs)))
    where putIndex = index' str

-- 50 random words generated
word' = ["trains", "mute", "quill", "injure", "effect", "value",
          "functional", "late","powder","previous","avoid","passenger",
          "grateful","building","hysterical","juggle","consider","legal",
          "thirsty","escape","amused","stuff","race","team","absurd","unadvised",
          "elegant","beautiful","marble","children","gray","secretary","attack",
          "subtract","unwritten","crib","degree","cynical","grouchy","abrasive",
          "afternoon","ignorant","disturbed","liquid","skillful","stove","cheerful",
          "trace","dreary","abhorrent"]

-- inserted all the words in new HashTable
hMap' = foldr (`insertStr` 0) hMap word'

-- calculate length of the string and stored as Int
lengthsTable = HashTable [fmap length <$> getHashTable hMap']

-- Definition of Sum monoid
newtype Sum a = Sum { getSum :: a } deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Semigroup (Sum a) where
     (Sum x) <> (Sum y) = Sum ( x + y )

instance (Num a) => Monoid (Sum a) where
     mempty = Sum 0
     mappend = (<>)

-- Show HashTable as an instance of Foldable
instance Foldable HashTable where
     foldMap g (HashTable[]) = mempty
     foldMap g (HashTable[x:xs]) = g x  `mappend` foldMap g (HashTable[xs])

-- stores the count of characters of lengthsTable
totalCharCount = getSum $ foldMap (\x -> (Sum x)) (mconcat $mconcat $getHashTable lengthsTable)
         
