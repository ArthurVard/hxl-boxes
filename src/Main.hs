
module Main where

import Boxes

main :: IO ()
main = do
  maybe (putStrLn "Nothing") print (calculateJumps [1,2,215,-3,6])
