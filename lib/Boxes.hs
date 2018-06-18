{-
  Created       : 2018 Jun 18 (Mon) 10:06:00 PM by Arthur Vardanyan.
  Last Modified : 2018 Jun 19 (Tue) 12:03:38 AM by Arthur Vardanyan.
-}

module Boxes where

calculateJumps :: [Int] -> Maybe Int
calculateJumps xs  = jump 0 (mkBoxes xs) []


calculateCycle :: [Int] -> Maybe Int
calculateCycle xs  = jumpO 0 (mkBoxes xs) []

mkBoxes :: [Int] -> [(Int, Int)]
mkBoxes xs = zip (zipWith (+) [0..] xs) [0..]

elemAt :: Int -> [a] -> [a]
elemAt _ [] = []
elemAt n (x:xs) | n < 0 = []
                | n == 0 = [x]
                | otherwise = elemAt (n-1) xs

jump :: Int -> [(Int, Int)] -> [Int] -> Maybe Int
jump _ []  _ = Nothing
jump n xs vs  = case (elemAt n xs) of
                  [(k,i)]  -> if checkCycle i vs
                              then Just $ length vs
                              else jump k xs (i:vs)
                  _       -> Nothing


jumpO :: Int -> [(Int, Int)] -> [Int] -> Maybe Int
jumpO _ []  _ = Nothing
jumpO n xs vs  = case (elemAt n xs) of
                  [(k,i)]  -> if checkCycle i vs
                              then Nothing
                              else jumpO k xs (i:vs)
                  _       -> Just $ length vs

checkCycle :: Int -> [Int] -> Bool
checkCycle i vs =  i `elem` vs
