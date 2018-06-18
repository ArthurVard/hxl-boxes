
module Main where

import Boxes

main :: IO ()
main = do
  maybe (putStrLn "Nothing") print (calculateJumps [1,2,215,-3,6])
  maybe (putStrLn "Nothing") print (calculateCycle [1,3,2,5])
