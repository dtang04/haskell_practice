module Main where
import GHC.Arr (accum)
import qualified Data.Set as S
import Data.Char (toLower, isDigit)

-- Function Exercises
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup [x] = [x]
dedup (x:y:xs)
    | x == y = dedup (y:xs)
    | otherwise = x : dedup (y:xs)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen cond l = go [] [] cond l
    where
        go current_lst current_chunk _ [] = current_lst ++ [current_chunk]
        go current_lst current_chunk cond (l:ls)
            | cond l == True = go (current_lst ++ [current_chunk]) [] cond ls
            | otherwise      = go current_lst (current_chunk ++ [l]) cond ls

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ z [] = [z]
scanl' f z lst = reverse (foldl build [z] lst)
    where
        build (b:bs) x = f b x : b : bs
-- foldl: (b -> a -> b) -> b -> [a] -> b
-- build: [b] -> a -> [b]
-- Here, b is the accumulator type, a is the next element of the list.
-- What is the accumulator here?
-- The accumulator is the list of results we have built so far, starting with [z].
-- How do we know what build is?
-- Consider scanl' (+) 0 [1,2,3] = reverse (foldl build [0] [1,2,3]) = build (build (build [0] 1) 2) 3
--                               -> build [0] 1 -> [1,0]
--                               -> build [1,0] 2 -> [3,1,0]
--                               -> build [3,1,0] 3 -> [6,3,1,0]
--                               -> reverse [6,3,1,0] -> [0,1,3,6]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f lst = reverse (foldl build [] lst)
    where
        build acc a = f a : acc

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd _ [] = []
dropWhileEnd f lst = reverse (go f [] True reverseLst)
    where
        reverseLst = reverse lst
        go _ acc _ [] = acc
        go f acc status (x:xs)
            | f x == True && status == True = go f acc status xs
            | otherwise                     = go f (acc ++ [x]) False xs

isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys)
    | x == y = isSubsequence xs ys
    | x /= y = isSubsequence (x:xs) ys
    | otherwise = False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' lst = foldr build [] lst
    where
        build x acc = acc ++ [x]

reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' lst = foldl build [] lst
    where
        build acc x = x : acc

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' f lst = foldl build ([], []) lst
    where
        build (pass, fail) x
            | f x == True = (pass ++ [x], fail)
            | otherwise   = (pass, fail ++ [x])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : (zipWith' f xs ys)

zipWithLeftOverLst1 :: (a -> b -> a) -> [a] -> [b] -> [a]
-- notice that to keep the left over elements from lst1, f must be a -> b -> a
-- The return type of zipWithLeftOverLst1 is [a], not [c]
zipWithLeftOverLst1 _ [] _ = []
zipWithLeftOverLst1 _ lst1 [] = lst1
zipWithLeftOverLst1 f (x:xs) (y:ys) = f x y : (zipWithLeftOverLst1 f xs ys)

zipSafe :: [a] -> [b] -> Maybe [(a,b)]
zipSafe a b = go a b []
    where
        go [] [] acc = Just (reverse acc)
        go [] _ _ = Nothing
        go _ [] _ = Nothing
        go (a:as) (b:bs) acc = go as bs ((a,b):acc)


find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' f (s:xs)
    |   f s == True = Just s
    |   otherwise   = find' f xs

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' a ((k,v):xs)
    | a == k     = Just v
    | otherwise  = lookup' a xs

lookupifEven' :: (Eq a, Integral b) => a -> [(a, b)] -> Maybe b
lookupifEven' a lst = 
    case lookup' a lst of
        Just k | even k -> Just k
        _               -> Nothing
-- Require b to be of Integral Typeclass so even can work correctly (even expects an integral)
-- If we want to force the association list to only have Int values, just change the type signature to 
-- Eq a => a -> [(a, Int)] -> Maybe Int (force b to not be any type)

lookupKV :: (Eq a, Eq b) => a -> b -> [(a,b)] -> Maybe (a,b)
lookupKV _ _ [] = Nothing
lookupKV a b ((k,v):xs)
    | a == k && b == v  = Just (a,b)
    | otherwise         = lookupKV a b xs

-- Types

data Point = Point Int Int deriving (Show)
-- Declare a new type with data Point (LHS)
-- Instantiate new Point constructor (RHS)
-- Point on LHS is a value constructor, Point on RHS is a type constructor

addPoints :: Point -> Point -> Point
addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2 ) (y1 + y2)

addListPoints :: [Point] -> Point
addListPoints lst = go lst (Point 0 0)
    where
        go [] acc = acc
        go (p:ps) acc = go ps (addPoints acc p)

data Result a
    = Ok a | Err String deriving (Show)
--When Haskell creates a Type constructor (Result), 
--it automatically creates value constructors for all values (Ok, Err)

mapResult :: (a -> b) -> Result a -> Result b
mapResult _ (Err a) = Err a --checks if a was created with the Err value constructor, unwrap a, and return Err a
mapResult f (Ok a) = Ok (f a) --checks if a was created with the Ok value constructor, unwrap a, and return Ok (f a)

-- Polymorphic Constructors
data Unlocked
data Locked

newtype Box state a = Box a deriving (Show)
-- data: Multiple constructors allowed, multiple fields allowed (e.g Point x y)
-- newtype: Shorthand of data, one constructor (box), one field (a)
-- Since a can be any type, we say that Box is a polymorphic type
-- LHS is a type constructor, RHS is a value constructor
-- state is a phantom type. A phantom type exists on the LHS but not the RHS.
-- A phantom type allows us to encode extra information in the type definition (e.g. Unlocked vs Locked)

createLock :: a -> Box Locked a
createLock = Box
-- originally lock x = Box x, but we can eta-reduce
-- Since a isn't of type Box, no unwrapping needs to be done. We simply wrap a to be Box a.
-- The type signature enforces Box a to be Box Locked a.

unlockBox :: Box Locked a -> Box Unlocked a
unlockBox (Box x) = Box x
-- checks if a was created with the Box value constructor, unwrap a, returns Box a. 
-- The type signature enforces Box a to be Box Unlocked a.

newtype Key a = Key String deriving (Show)
-- Here, a is a phantom type. 
-- An unwrapped Key must be a string.

unlockBox' :: Box Locked a -> Key b -> String -> Maybe (Box Unlocked a)
unlockBox' (Box x) (Key y) key
    | y == key = Just (Box x)
    | otherwise = Nothing
-- a is an object with any type, b is a phantom type.
-- We unwrap y to access the string, and compare with a string.
-- We unwrap x to change the Box from Locked to Unlocked if y == k
-- We return an unlocked Box containing a.  We wrap with Maybe because we return Nothing if the key is not correct.

lockBox :: Box Unlocked a -> Box Locked a
lockBox (Box x) = Box x

data Fresh
data Used

newtype Token state = Token String deriving (Show)

newToken :: String -> Token Fresh
newToken = Token --originally newToken x = Token x

useToken :: Token Fresh -> Token Used
useToken (Token x) = Token x

tokenValue :: Token Used -> String
tokenValue (Token k) = k

compareTokens :: Token Used -> Token Used -> Bool
compareTokens (Token a) (Token b)
    | a == b    = True
    | otherwise = False

data Meters
data Feet

newtype Distance unit = Distance Double deriving (Show)

meters :: Double -> Distance Meters
meters = Distance

feet :: Double -> Distance Feet
feet = Distance

add :: Distance p -> Distance p -> Distance p 
--the phantom type (p) enforces that distances added must be either 
--Distance Meters + Distance Meters or Distance Feet + Distance Feet
add (Distance a) (Distance b) = Distance(a + b) 
    
feetToMeters :: Distance Feet -> Distance Meters
feetToMeters (Distance a) = Distance (a * 0.3048)

checkSame :: Distance Feet -> Distance Meters -> Bool
checkSame f (Distance m) = 
    case feetToMeters f of 
        Distance f | f == m -> True
        _                    -> False
-- Case does pattern matching based on the type.
-- Guards refine the pattern further

metersEqual :: Distance Meters -> Distance Meters -> Bool
metersEqual a b = 
    case (a, b) of
        --case pattern matching of two vars, a b
        (Distance x, Distance y) | x == y -> True
        _                                 -> False

isZero :: Distance u -> Bool
isZero (Distance a)
    | a == 0        = True
    | otherwise     = False

pairwiseEqualMeters :: [Distance Feet] -> [Distance Meters] -> Bool
pairwiseEqualMeters [] [] = True
pairwiseEqualMeters [] _ = False
pairwiseEqualMeters _ [] = False
pairwiseEqualMeters f m
    | length f == length m = 
        let 
            bool_lst = zipWith checkSame f m 
        in go bool_lst
    | otherwise = False
    where 
        -- where must come at the end of the function definition, not in between guards
            go [] = True
            go (b:bs)
                | b == False = False
                | otherwise  = go bs 

newtype Pair a b = Pair (a, b)
-- We need to specify a b on the LHS (while we didn't for Distance)
-- This is because a and b are polymorphic types, while Distance just took in Double
-- General rule: Any unspecified types associated with the new type, either phantom or polymorphic, must be declared on the LHS.

-- Typeclasses
instance Eq (Distance u) where
    (==) x y = 
        case (x, y) of
            (Distance x, Distance y) | x Prelude.== y -> True
            _                                 -> False

instance Ord (Distance u) where
    -- compare :: Ord a => a -> a -> Ordering
    -- data Ordering = LT | EQ | GT
    compare x y =
        case (x, y) of
            (Distance x, Distance y) | x Prelude.< y  -> LT
            (Distance x, Distance y) | x Prelude.== y -> EQ
            (Distance x, Distance y) | x Prelude.> y  -> GT
            _                                         -> error "error"

instance (Eq a, Eq b) => Eq (Pair a b) where
    (==) (Pair (a, b)) (Pair (a', b'))
        |  a Prelude.== a' && b Prelude.== b' = True
        |  otherwise                          = False
    
    
instance (Ord a, Ord b) => Ord (Pair a b) where
    compare x y = 
        case (x, y) of
            (Pair (a, _), Pair (a', _)) | a Prelude.< a' -> LT
            (Pair (a, _), Pair (a', _)) | a Prelude.> a' -> GT
            (Pair (a, b), Pair (a', b')) | a Prelude.== a' && b < b' -> LT
            (Pair (a, b), Pair (a', b')) | a Prelude.== a' && b > b' -> GT
            (Pair (a, b), Pair (a', b')) | a Prelude.== a' && b == b' -> EQ
            _                                                         -> error "error"

instance (Show a, Show b) => Show (Pair a b) where
    show (Pair (a, b)) = "(" ++ Prelude.show a ++ ", " ++ Prelude.show b ++ ")" 

-- Sets
s :: S.Set Int

-- The name followed by "::" typically denotes the type of the object

-- Set Functions
s = S.fromList [1,2,2,2,3,3,3]
s' = S.insert 4 s
isIn = S.member 4 s'
lst = S.toList s'
onlyEvensSet = S.filter even s'
add1Set = S.map (+1) s'

-- Exercises
listtoLower :: [String] -> [String]
listtoLower [] = []
listtoLower x = map (map toLower) x

-- listtoLower (x:xs) = map toLower x : listtoLower xs
-- map toLower is String -> String
-- the outer map maps the inner (map toLower) to every element in the List
listtoLowerLargeWords :: [String] -> [String]
listtoLowerLargeWords [] = []
listtoLowerLargeWords lst =
    let
        largeWords = filter (\x -> length x > 3) lst
    in map (map toLower) largeWords
-- \x - \ is the lambda function declaration, x is the argument

filterAssocList :: (Ord a, Ord b) => [(a, b)] -> (a, b) -> [(a, b)]
filterAssocList [] _ = []
filterAssocList (x:xs) y
    | x > y         = x:filterAssocList xs y -- use 2-tuple comparison
    | otherwise     = filterAssocList xs y

filterPositives :: [(Double, Double)] -> [(Double, Double)]
filterPositives [] = []
filterPositives assocList = filterAssocList assocList (0, 0)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = go x 2
    where
        go x acc
            | acc >= x - 1      = True
            | x `mod` acc == 0  = False
            | otherwise         = go x (acc+1)

isPrimeFilter :: [Int] -> [Int]
isPrimeFilter = filter isPrime

checkDigit :: String -> Bool
checkDigit [] = False
checkDigit (x:xs) 
    | isDigit x  = True
    | otherwise  = checkDigit xs

filter' :: [String] -> [Int]
filter' [] = []
filter' lst = 
    let
        lst' = map (map toLower) lst
        lst'' = filter (\x -> length x >= 3) lst'
        lst''' = map length (filter (not . checkDigit) lst'')
    in filter isPrime lst'''
        
-- Takes a list of strings
-- Converts each string to lowercase
-- Removes any string that:
    -- is shorter than 3 characters
    -- contains a digit
-- Converts each remaining string to its length
-- Keeps only lengths that are prime numbers


main :: IO ()
main = do
    print (dedup [1,1,2,2,3,3,3,4,4,5])
    print (splitWhen (== ',') "a,b,")
    print (scanl' (+) 0 [1,2,3,4])
    print (map' (*2) [1,2,3,4])
    print (dropWhileEnd (== 0) [1,2,3,0,0])
    print (isSubsequence "abc" "aebdc")
    print (reverse' [1,2,3,4,5])
    print (reverse'' [5,6,7,8,9])
    print (partition' even [1..10])
    print (zipWith' (*) [1,2,3] [4,5,6,7])
    print (zipWith' (*) [1,2,4,5,6] [4,5,6,7])
    print (zipWithLeftOverLst1 (*) [1,2,3,4,5] [10,20])
    print (addPoints (Point 1 2) (Point 3 4))
    print (addListPoints [Point 1 2, Point 3 4, Point 5 6])
    print (mapResult (+1) (Ok 3))
    print (mapResult (+1) (Err "Error"))
    let 
        k = Key "hello" :: Key () -- () fills the phantom parameter
        box = Box "secret" :: Box Locked String -- Cast Box to Box Locked String
    print (unlockBox' box k "hello")
    print (unlockBox' box k "wrong key")
    let
        t1 = Token "token1" :: Token Fresh
        t2 = Token "token2" :: Token Fresh
        t1' = useToken t1
        t2' = useToken t2
    print (tokenValue t1')
    print (tokenValue t2')
    print (compareTokens t1' t2')
    let
        dist1 = Distance 1 :: Distance Feet
        dist2 = Distance 0.3048 :: Distance Meters
        dist3 = Distance 0.3048 :: Distance Meters
        dist4 = Distance 1 :: Distance Meters
    print (add (feetToMeters dist1) dist2)
    print (checkSame dist1 dist2)
    print (metersEqual dist2 dist3)
    print (isZero dist1)
    print (pairwiseEqualMeters [Distance 1, Distance 2] [Distance 0.3048, Distance 0.6096])
    print (zipSafe [1,2,3] ['a','b','c'])
    print (zipSafe [1,2,3,4] ['a','b','c'])
    print (dist2 == dist3) -- testing overriding (==) for distance classes
    print (dist3 == dist4)
    print (dist3 < dist4)
    print (lookup' "d" [("a", 1), ("b", 2), ("c", 3), ("d", 4)])
    print (lookupifEven' "a" [("a", 1), ("b", 2), ("c", 3), ("d", 4)])
    print (lookupifEven' "b" [("a", 1), ("b", 2), ("c", 3), ("d", 4)])
    print (lookupKV "a" 1 [("a", 1), ("b", 2), ("c", 3), ("d", 4)])
    print (Pair (3, 4) == Pair (3, 4))
    print (Pair (4, 5) < Pair (4, 6))
    print (Pair ("a", "b") > Pair ("c", "d"))
    print (Pair ("a", "b"))
    print s
    print s'
    print isIn
    print lst
    print onlyEvensSet
    print add1Set
    print (listtoLower ["abcD", "a", "b", "C", "abCEdf", "abc"])
    print (listtoLowerLargeWords ["abcD", "a", "b", "C", "abCEdf", "abc"])
    let
        assocList1 = [("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5)]
        assocList2 = [(-1, -1), (-2, 5), (0, 0), (1, 1), (3, 4), (-7, 2)]
    print (filterAssocList assocList1 ("c", 4))
    print (filterPositives assocList2)
    print (isPrimeFilter [1..100])
    print (filter' ["Hi", "Hello2", "WORLD", "abc", "abcd", "Abcdefg"])