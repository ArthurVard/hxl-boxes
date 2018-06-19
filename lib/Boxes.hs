{-
  Created       : 2018 Jun 18 (Mon) 10:06:00 PM by Arthur Vardanyan.
  Last Modified : 2018 Jun 19 (Tue) 10:25:35 PM by Arthur Vardanyan.
-}

module Boxes
    ( calculateJumps
    , elemAt) where

import Data.List (findIndex)
import qualified  Data.IntMap.Strict as Map
import qualified Data.Set as Set

type Index   = Int
type JumpTo = Int

-- | The 'calculateJumps' function finds the number of jumps before
--   jumping out of the board
--
-- >>> calculateJumps [3,-1,6,-2,15,8]
-- Nothing
-- >>> calculateJumps [3,-1,6,-22,15,8]
-- Just 1
-- >>> calculateJumps [1,2,10,0]
-- Nothing
calculateJumps :: [Int] -> Maybe Int
calculateJumps xs  = jump 0 (mkBoxes xs) []
    where
      jump :: Int -> [(JumpTo, Index)] -> [Index] -> Maybe Int
      jump _ [] vs = Just $ length vs - 1
      jump n zs vs = case (elemAt n zs) of
                       [(k,i)]  -> if i `elem` vs
                                   then Nothing
                                   else jump k zs (i:vs)
                       _       -> Just $ length vs - 1


-- | The 'mkBoxes' funcion creates more convenient data structure to process
--
-- >>> mkBoxes [1,2,3,4]
-- [(1,0),(3,1),(5,2),(7,3)]
-- >>> mkBoxes [1,2,-6,4]
-- [(1,0),(3,1),(-4,2),(7,3)]
mkBoxes :: [Int] -> [(JumpTo, Index)]
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

-------------------------------------------------------------------------------
-- simple solution

-- >>> calculateJumps'' [3,-1,6,-2,15,8]
-- Nothing
-- >>> calculateJumps'' [3,-1,6,-22,15,8]
-- Just 1
-- >>> calculateJumps'' [1,2,10,0]
-- Nothing

calculateJumps'' :: [Int] -> Maybe Int
calculateJumps'' xs  = jump 0 (mkMap xs) 0
    where
      listN = length xs
      jump :: Int -> Map.IntMap Int -> Int -> Maybe Int
      jump n zs count  = case (Map.lookup n zs) of
                           Just k  -> if count > listN
                                      then Nothing
                                      else jump k zs (count + 1)
                           _       -> Just count



-------------------------------------------------------------------------------
-- a bit Optimized version of my solution


mkMap :: [Int] -> Map.IntMap Int
mkMap = Map.fromList . mkBoxes


calculateJumps' :: [Int] -> Maybe Int
calculateJumps' xs  = jump 0 (mkMap xs) Set.empty
    where
      jump :: Int -> Map.IntMap Int -> Set.Set Int -> Maybe Int
      jump n zs vs = case (Map.lookup n zs) of
                       Just k  -> if Set.member k  vs
                                  then Nothing
                                  else jump k zs (Set.insert k vs)
                       _       -> Just $ Set.size vs


mlength :: Eq a => [a] -> Maybe Int
mlength [] = Nothing
mlenght (x:xs) =
  let loop _ _ _ [] = Nothing
      loop pow lam x (y:ys)
        | x == y     = Just lam
        | pow == lam = loop (2*pow) 1 y ys
        | otherwise  = loop pow (1+lam) x ys
  in loop 1 1 x xs

-------------------------------------------------------------------------------
-- Stream playgroun

infix 5 :.
data Stream a = a :. Stream  a


streamToList :: Stream a -> [a]
streamToList (s:.ss) = s : streamToList ss

instance Show a => Show (Stream a) where
--    show = show . take 20 . streamToList
    show ss = show' ss 20
        where
          show' :: Show a => Stream a -> Integer -> String
          show' _ 0 = "..."
          show' (e:.s) n = show e ++","++ show' s (n-1)



-------------------------------------------------------------------------------
-- Floyd's cycle-finding algorithm and Brent's Cycle algorithm

-- https://wiki.haskell.org/Floyd's_cycle-finding_algorithm
findCycle' :: Eq a => [a] -> ([a],[a])
findCycle' xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x == y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys


-- | Brent's Cycle algorithm
-- https://rosettacode.org/wiki/Cycle_detection#Haskell
--
findCycle :: Eq a => [a] -> Maybe ([a], Int, Int)
findCycle lst =
  do l <- findCycleLength lst
     mu <- findIndex (uncurry (==)) $ zip lst (drop l lst)
     let c = take l $ drop mu lst
     return (c, l, mu)


findCycleLength :: Eq a => [a] -> Maybe Int
findCycleLength [] = Nothing
findCycleLength (x:xs) =
  let loop _ _ _ [] = Nothing
      loop pow lam x (y:ys)
        | x == y     = Just lam
        | pow == lam = loop (2*pow) 1 y ys
        | otherwise  = loop pow (1+lam) x ys
  in loop 1 1 x xs
