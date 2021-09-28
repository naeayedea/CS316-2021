{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Problems where

import Week02 hiding (removeAll) --need to hide removeAll from notes

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them.-}

{- 1. Write a function that counts t he number of occurrences of an
      element in list: -}

--first way that came to mind (best way for this problem)

popCount :: Eq a => a -> [a] -> Int
popCount x [] = 0
popCount x (y:ys) 
       | x == y    = 1 + popCount x ys
       | otherwise = popCount x ys


--the next two popCount implementations are just to get practice with
--case statements and if else 

popCount0 :: Ord a => a -> [a] -> Int
popCount0 x [] = 0
popCount0 x (y:ys) = 
      case compare x y of
            EQ -> 1 + popCount0 x ys
            _  -> popCount0 x ys

--with if else 
popCount1 :: Ord a => a -> [a] -> Int
popCount1 x [] = 0
popCount1 x (y:ys) = 
      if x == y 
            then 1 + popCount1 x ys
            else popCount1 x ys


{-    (popCount is short for "population count"). Examples:

         popCount 2 [1,2,5,2,7,2,9] == 3
         popCount 9 [1,2,5,2,7,2,9] == 1
         popCount 0 [1,2,5,2,7,2,9] == 0
-}


{- 2. Write a version of 'insert' that only inserts into a sorted list
      if the element is not already there. Examples:

         insertNoDup 2 [1,3,4]   == [1,2,3,4]
         insertNoDup 2 [1,2,3,4] == [1,2,3,4]
-}

insertNoDup :: Ord a => a -> [a] -> [a]
insertNoDup x [] = [x]
insertNoDup x (y:ys) 
      | x == y    = y:ys
      | x < y     = x : y : ys
      | otherwise = y : insertNoDup x ys


{- 3. Write a version of 'remove' that removes all copies of an element
      from a sorted list, not just the first one. Examples:

         removeAll 2 [1,2,2,3] == [1,3]
         removeAll 2 [1,3]     == [1,3]
-}

--already done this when taking notes for Week02.hs, copied from there: 

removeAll :: Ord a => a -> [a] -> [a]
removeAll y [] = []
removeAll y (x:xs)
  | x == y = removeAll y xs
  | otherwise = x : removeAll y xs


{- 4. Rewrite 'treeFind' and 'treeInsert' to use 'compare' and 'case'
      expressions. -}

treeFind2 :: Ord k => k -> KV k v -> Maybe v
treeFind2 x Leaf = Nothing
treeFind2 x (Node left (key, value) right) =
      case compare x key of
            EQ -> Just value
            GT -> treeFind2 x right 
            LT -> treeFind2 x left 

treeInsert2 :: Ord k => k -> v -> KV k v -> KV k v
treeInsert2 k' v' Leaf                     = Node Leaf (k',v') Leaf
treeInsert2 k' v' (Node left (k, v) right) =
      case compare k' k of
            EQ -> Node left    (k', v')    right
            GT -> Node left    (k , v )    newRight
            LT -> Node newLeft (k , v )    right
            where 
                  newRight = treeInsert2 k' v' right
                  newLeft  = treeInsert2 k' v' left 


{- 5. MergeSort is another sorting algorithm that works in the following
      way:

      - If the list to be sorted is zero length, then it is already
        sorted.

      - If the list to be sorted has one element, then it is already
        sorted.

      - Otherwise, split the list into two, one with the even elements
        and one with the odd elements. Sort the two lists by calling
        'mergeSort' recursively. Then merge the two lists together
        maintaining the ordering.

      Write this function in three parts: -}

{-    'split' splits the input into two lists: one with the odd numbered
      elements and one with the even numbered elements. HINT: you can
      pattern match on multiple elements at the head of a list with
      'x1:x2:xs', and you can use the '(odds,evens) = ...' syntax in a
      'where' clause. -}

split :: Ord a => [a] -> ([a], [a])
split []         = ([],[])
split [x]     = ([x], [])
split (x1:x2:xs) = (x1:odds, x2:evens)
      where
            (odds, evens) = split xs 
      

{-    'merge' merges two sorted lists into one sorted list. Examples:

          merge [1,3,5] [2,4,6]  = [1,2,3,4,5,6]
          merge [1,3,5] [7,9,11] = [1,3,5,7,9,11]
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) 
      | x < y     = x : merge xs (y:ys)
      | x == y    = x : y : merge xs ys
      | otherwise = y : merge (x:xs) ys


{-    'mergeSort' uses 'split' and 'merge' to implement the merge sort
      algorithm described above. -}

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort odds) (mergeSort evens)
      where (odds,evens) = split list 


{- 6. Write another version of 'makeChange' that returns all the
      possible ways of making change as a list: -}

makeChangeAll :: [Coin] -> [Coin] -> Int -> [[Coin]]
makeChangeAll coins        used 0 = [used]
makeChangeAll []           used x = []
makeChangeAll (coin:coins) used x
      {-if amount is greater than coin, subtract from amount 
        and add coin to used like normal-}
      | x >= coin = makeChangeAll coins (coin:used) (x - coin)
                 {-to get all possibilities, repeat the previous process,
                   but by trying a different route e.g. don't use the
                   current coin and instead try the next with the same
                   value and append it to the current attempt-}
                 ++ makeChangeAll coins  used        x
      |otherwise  = makeChangeAll coins  used        x

{- HINT: you don't need a case expression, just a way of appending two
   lists of possibilities. -}
