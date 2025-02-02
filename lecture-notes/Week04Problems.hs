{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week04Problems where

import Prelude hiding (foldr, foldl, Maybe (..), Left, Right, filter, zip, map, concat)
import Data.List.Split (splitOn)
import Data.List hiding (foldr, foldl, filter, map, concat)
import Week04
import GHC.Arr (badSafeIndex)

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. The following recursive function returns the list it is given as
      input: -}

listIdentity :: [a] -> [a]
listIdentity []     = []
listIdentity (x:xs) = x : listIdentity xs

{- Write this function as a 'foldr' (fill in the 'undefined's): -}

listIdentity' :: [a] -> [a]
listIdentity' = foldr (:) []


{- 2. The following recursive function does a map and a filter at the
      same time. If the function argument sends an element to
      'Nothing' it is discarded, and if it sends it to 'Just b' then
      'b' is placed in the output list. -}

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x:xs) = case f x of
                       Nothing -> mapFilter f xs
                       Just b  -> b : mapFilter f xs

{- Write this function as a 'foldr': -}

mapFilter' :: (a -> Maybe b) -> [a] -> [b]
mapFilter' f = foldr step []
   where step = \element list -> case f element of
                                    Nothing -> list
                                    Just b -> b : list


{- 3. Above we saw that 'foldl' and 'foldr' in general give different
      answers. However, it is possible to define 'foldl' just by using
      'foldr'.

      First try to define a function that is the same as 'foldl',
      using 'foldr', 'reverse' and a '\' (STUDY NOTE: '\' = lambda) function: -}

{- foldr type signature and code: 
   foldr :: (a -> b -> b) -> b -> [a] -> b
   foldr f a []     = a
   foldr f a (x:xs) = f x (foldr f a xs)

   foldl type signature and code: 

   foldl :: (b -> a -> b) -> b -> [a] -> b
   foldl f a []     = a
   foldl f a (x:xs) = foldl f (f a x) xs
-}

foldlFromFoldrAndReverse :: (b -> a -> b) -> b -> [a] -> b
foldlFromFoldrAndReverse f x xs = foldr (\a b -> f b a) x (reverse xs)

{-  We could have also used the 'flip' function from last week's
    questions, which is provided by the standard library: -}

foldlFromFoldrAndReverse_v2 :: (b -> a -> b) -> b -> [a] -> b
foldlFromFoldrAndReverse_v2 f x xs = foldr (flip f) x (reverse xs)


{-   Much harder: define 'foldl' just using 'foldr' and a '\' function: -}

foldlFromFoldr :: (b -> a -> b) -> b -> [a] -> b
foldlFromFoldr f x xs = undefined


{- 4. The following is a datatype of Natural Numbers (whole numbers
      greater than or equal to zero), represented in unary. A natural
      number 'n' is represented as 'n' applications of 'Succ' to
      'Zero'. So '2' is 'Succ (Succ Zero)'. Using the same recipe we
      used above for 'Tree's and 'Maybe's, work out the type and
      implementation of a 'fold' function for 'Nat's. -}

data Nat
  = Zero
  | Succ Nat
  deriving Show

{-STUDY NOTE: Succ is of type (Nat -> Nat) -}

{- HINT: think about proofs by induction. A proof by induction has a
   base case and a step case. -}

foldNat :: (b -> b) -> b -> Nat -> b
foldNat succ zero Zero     = zero
foldNat succ zero (Succ n) = succ (foldNat succ zero n)

{- Here we have 'zero' for the base case, 'succ' for the step case.

   As an example, we can define 'add' for 'Nat' in terms of 'foldNat',
   which has a similar structure to 'append' for lists: -}

add :: Nat -> Nat -> Nat
add x y  = foldNat Succ y x

{- 5. Write a list comprehension to generate all the cubes (x*x*x) of
      the numbers 1 to 10: -}

cubes :: [Int]
cubes = [x^3 | x <- [1..10]]


{- 6. The replicate function copies a single value a fixed number of
      times:

         > replicate 5 'x'
         "xxxxx"

      Write a version of replicate using a list comprehension: -}

replicate' :: Int -> a -> [a]
replicate' n s = [s | y <- [1..n]]

{- 7. One-pass Average.

   It is possible to use 'foldr' to
   implement many other interesting functions on lists. For example
   'sum' and 'len': -}

sumDoubles :: [Double] -> Double
sumDoubles = foldr (\x sum -> x + sum) 0

lenList :: [a] -> Integer
lenList = foldr (\_ l -> l + 1) 0

{- Putting these together, we can implement 'avg' to compute the average
   (mean) of a list of numbers: -}

avg :: [Double] -> Double
avg xs = sumDoubles xs / fromInteger (lenList xs)

{- Neat as this function is, it is not as efficient as it could be. It
   traverses the input list twice: once to compute the sum, and then
   again to compute the length. It would be better if we had a single
   pass that computed the sum and length simultaneously and returned a
   pair.

   Implement such a function, using foldr: -}

sumAndLen :: [Double] -> (Double, Integer)
sumAndLen = foldr (\x (sum, len) -> (sum + x, len + 1)) (0,0)

{- Once you have implemented your 'sumAndLen' function, this alternative
   average function will work: -}

avg' :: [Double] -> Double
avg' xs = total / fromInteger length
  where (total, length) = sumAndLen xs

{- 8. mapTree from foldTree

   Here is the 'Tree' datatype that is imported from the Week04 module:

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

   As we saw in the lecture notes, it is possible to write a generic
   recursor pattern for trees, similar to 'foldr', copied here for reference:

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf           = l
foldTree l n (Node lt x rt) = n (foldTree l n lt) x (foldTree l n rt)

   Your job is to implement 'mapTree' (from Week03) in terms of
   'foldTree': -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\l x r -> Node l (f x) r )

{- Here is the explicitly recursive version of 'mapTree', for
   reference: -}

mapTree0 :: (a -> b) -> Tree a -> Tree b
mapTree0 f Leaf           = Leaf
mapTree0 f (Node lt x rt) = Node (mapTree0 f lt) (f x) (mapTree0 f rt)

{- 9. Finally, use 'foldTree' to flatten a tree to list in left-to-right
   order: -}

flatten :: Tree a -> [a]
flatten = foldTree [] (\l x r -> l ++ [x] ++ r)