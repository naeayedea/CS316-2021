module Week01Problems where

import Week01
import Prelude hiding (Left, Right, reverse)

{----------------------------------------------------------------------}
{- Tutorial Questions                                                 -}
{----------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them.-}

{- 1. Write a function: -}

isHorizontal :: Direction -> Bool
isHorizontal Left = True 
isHorizontal Right = True
isHorizontal _ = False

{- that returns 'True' if the direction is 'Left' or 'Right', and
   'False' otherwise. -}


{- 2. Write a function: -}

flipHorizontally :: Direction -> Direction
flipHorizontally Left = Right 
flipHorizontally Right = Left 
flipHorizontally d = d

{- that flips horizontally (Left <-> Right, Up and Down stay the same). -}


{- 3. Rewrite 'equalDirections' to take a 'Pair Direction Direction' as
      input: -}

pairOfEqualDirections :: Pair Direction Direction -> Bool
pairOfEqualDirections (MkPair Left Left) = True 
pairOfEqualDirections (MkPair Right Right) = True
pairOfEqualDirections (MkPair Up Up) = True 
pairOfEqualDirections (MkPair Down Down) = True
pairOfEqualDirections (MkPair _ _) = False


{- 4. Define a datatype 'Triple a b c' for values that have three
      components. Write functions 'get1of3 :: Triple a b c -> a',
      'get2of3' and 'get3of3' that return the first, second and third
      components. You will have to come up with the type signatures
      for the second and third one. -}

data Triple a b c = MkTriple a b c
      deriving Show

get1of3 :: Triple a b c -> a
get1of3 (MkTriple a b c) = a

get2of3 :: Triple a b c -> b
get2of3 (MkTriple a b c) = b

get3of3 :: Triple a b c -> c
get3of3 (MkTriple a b c ) = c



{- 5. Pattern matching on specific characters is done by writing the
      character to match. For example: -}

isA :: Char -> Bool
isA 'A' = True
isA _   = False

{-    Write a function 'dropSpaces' :: [Char] -> [Char]' that drops
      spaces from the start of a list of characters. For example, we
      should have:

         *Week01> dropSpaces "   hello"
         "hello"

      (Strings in Haskell are really lists of 'Char's) -}

dropSpaces :: [Char] -> [Char]
dropSpaces [] = [] 
dropSpaces (' ':xs) = dropSpaces xs
dropSpaces (x:xs) = x:xs

{- 6. Using 'reverse' and 'dropSpaces', write a function that removes
      spaces at the *end* of a list of characters. For example:

         *Week10> dropTrailingSpaces "hello    "
         "hello"
-}

dropTrailingSpaces :: [Char] -> [Char]
dropTrailingSpaces string = reverse (dropSpaces (reverse string))
