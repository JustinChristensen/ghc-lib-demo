module Main where

-- import Bar (sayHi)
import Data.Text (Text, pack)
import qualified Data.Text.IO as I (putStrLn)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs :: [Int]
fibs = fib <$> [0..]

main :: IO ()
main = do
    I.putStrLn $ pack "First 10 fibs"
    I.putStrLn $ pack $ unlines $ show <$> take 10 fibs


