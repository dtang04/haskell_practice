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

main :: IO ()
main = do
    print (dedup [1,1,2,2,3,3,3,4,4,5])
    print (splitWhen (== ',') "a,b,")
