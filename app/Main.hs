module Main where

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
-- What is the accumulator here?
-- The accumulator is the list of results we have built so far, starting with [z].
-- How do we know what build is?
-- Consider scanl' (+) 0 [1,2,3] -> build (build(build [0] 1) 2) 3
--                               -> build [0] 1 must be [0,1]
--                               -> build [0,1] 2 must be [0,1,3]x
main :: IO ()
main = do
    print (dedup [1,1,2,2,3,3,3,4,4,5])
    print (splitWhen (== ',') "a,b,")
    print (scanl' (+) 0 [1,2,3,4])
