module Main where

import Lib ( translate )


main :: IO ()
main = do
    translate
    putStrLn "変換終了"
    readLn