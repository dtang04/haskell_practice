module Main where
import GHC.Arr (accum)

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