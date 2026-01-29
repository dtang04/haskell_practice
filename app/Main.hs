module Main where
import GHC.Arr (accum)

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
    