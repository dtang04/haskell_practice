module Main where
import System.Environment (getArgs)

main :: IO()
main = do
    input <- getContents    -- getContents takes in input until an EOF (Ctrl+D) is parsed.
                            -- returns IO string, unwrap IO monad with <-
    let
        words_lst = words input --can't have assignment (=) inside do
        lines_lst = lines input
    putStrLn ("# chars: " ++ show (length input)) --print calls show, but we just want to print the string as-is, so use putStrLn
    putStrLn ("# lines: " ++ show (length lines_lst))
    putStrLn ("# words " ++ show (length words_lst))