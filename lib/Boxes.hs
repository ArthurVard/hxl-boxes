{-
  Created       : 2018 Jun 18 (Mon) 10:06:00 PM by Arthur Vardanyan.
  Last Modified : 2018 Jun 19 (Tue) 01:55:09 AM by Arthur Vardanyan.
-}

module Boxes
    ( calculateJumps
    , elemAt) where

type Index   = Int
type PointTo = Int

-- | The 'calculateJumps' function finds the number of jumps before
--   jumping out of the board
--
-- >>> calculateJumps [3,-1,6,-2,15,8]
-- Nothing
-- >>> calculateJumps [3,-1,6,-22,15,8]
-- Just 2
-- >>> calculateJumps [1,2,10,0]
-- Nothing
calculateJumps :: [Int] -> Maybe Int
calculateJumps xs  = jump 0 (mkBoxes xs) []
    where
      jump :: Int -> [(PointTo, Index)] -> [Index] -> Maybe Int
      jump _ [] vs = Just $ length vs
      jump n zs vs = case (elemAt n zs) of
                       [(k,i)]  -> if i `elem` vs
                                   then Nothing
                                   else jump k zs (i:vs)
                       _       -> Just $ length vs


-- | The 'mkBoxes' funcion creates more convenient data structure to process
--
-- >>> mkBoxes [1,2,3,4]
-- [(1,0),(3,1),(5,2),(7,3)]
-- >>> mkBoxes [1,2,-6,4]
-- [(1,0),(3,1),(-4,2),(7,3)]
mkBoxes :: [Int] -> [(PointTo, Index)]
mkBoxes xs = zip (zipWith (+) [0..] xs) [0..]



-- | The 'elemAt' function returns element at given index as a one element list or empty list if
-- index is negative or exceeds list bound.
--
-- >>> let xs = [0..10]
-- >>> elemAt 0 xs
-- [0]
-- >>> elemAt 3 xs
-- [3]
-- >>> elemAt 13 xs
-- []
-- >>> elemAt (-6) xs
-- []
elemAt :: Index -> [a] -> [a]
elemAt _ [] = []
elemAt n (x:xs) | n < 0 = []
                | n == 0 = [x]
                | otherwise = elemAt (n-1) xs
