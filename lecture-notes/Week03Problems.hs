module Week03Problems where

import Data.Char

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. Lambda notation.

   Rewrite the following functions using the '\x -> e' notation (the
   "lambda" notation), so that they are written as 'double =
   <something>', and so on. -}

mulBy2 :: Int -> Int
mulBy2 x = 2*x

mulBy2' :: Int -> Int 
mulBy2' = \x -> x * 2

mul :: Int -> Int -> Int
mul x y = x * y

mul' :: Int -> Int -> Int
mul' = \x y -> x * y

invert :: Bool -> Bool
invert True  = False
invert False = True
  {- HINT: use a 'case', or an 'if'. -}

invert' :: Bool -> Bool 
invert' = \b -> not b 
-- or invert' = \b -> if b then False else True 


{- 2. Partial Application

   The function 'mul' defined above has the type 'Int -> Int ->
   Int'. (a) What is the type of the Haskell expression:

       mul 10

   A: mul 10 has type Int -> Int 

   (b) what is 'mul 10'? How can you use it to multiply a number? 
   
   A: mul 10 is a function with type (Int -> Int), applying it to a 
   number evaluates to mul 10 num e.g. Int -> Int -> Int
   -}

{- 3. Partial Application

   Write the 'mulBy2' function above using 'mul'. Can you make your
   function as short as possible? -}

double_v2 :: Int -> Int
double_v2 = mul 2

{- 4. Using 'map'.

   The function 'toUpper' takes a 'Char' and turns lower case
   characters into upper cases one. All other characters it returns
   unmodified. For example:

       > toUpper 'a'
       'A'
       > toUpper 'A'
       'A'

   Strings are lists of characters. 'map' is a function that applies a
   function to every character in a list and returns a new list.

   Write the function 'shout' that uppercases a string, so that:

      > shout "hello"
      "HELLO"
-}

shout :: String -> String    -- remember that String = [Char]
shout = map toUpper 

{- 5. Using 'map' with another function.

   The function 'concat' does what the function 'concatLists' from
   Exercise 1 does, but is built in to the library:

      > concat [[1,2],[3,4],[5,6]]
      [1,2,3,4,5,6]

   Using 'map', 'concat', and either a helper function or a function
   written using '\', write a function 'dupAll' that duplicates every
   element in a list. For example:

      > dupAll [1,2,3]
      [1,1,2,2,3,3]
      > dupAll "my precious"
      "mmyy  pprreecciioouuss"

   HINT: try writing a helper function that turns single data values
   into two element lists. -}

dupAll :: [a] -> [a]
dupAll = concat . map (\value -> valToList value) 

valToList :: a -> [a]
valToList v = [v, v]


{- 6. Using 'filter'

   (a) Use 'filter' to return a list consisting of only the 'E's in
       a 'String'.

   (b) Use 'onlyEs' and 'length' to count the number of 'E's in a string.

   (c) Write a single function that takes a character 'c' and a string
       's' and counts the number of 'c's in 's'. -}

onlyEs :: String -> String
onlyEs = filter (\char -> char == 'E')

numberOfEs :: String -> Int
numberOfEs = length . onlyEs

numberOf :: Char -> String -> Int
numberOf = \c -> length . (filter (\char -> char == c))


{- 7. Rewriting 'filter'

   (a) Write a function that does the same thing as filter, using
      'map' and 'concat'.

   (b) Write a function that does a 'map' and a 'filter' at the same
       time, again using 'map' and 'concat'.
-}

filter_v2 :: (a -> Bool) -> [a] -> [a]
filter_v2 = \f -> concat . (map (\x -> if f x then [x] else [])) 

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = \f -> concat . map (\x ->  case f x of 
                                       Nothing -> []
                                       Just y  -> [y])


{- 8. Evaluating Formulas

   Here is a datatype describing formulas in propositional logic, as
   in CS208 last year. Atomic formulas are represented as 'String's. -}

data Formula
  = Atom String
  | And  Formula Formula
  | Or   Formula Formula
  | Not  Formula
  deriving Show

{- (a) Write a function that evaluates a 'Formula' to a 'Bool'ean value,
       assuming that all the atomic formulas are given the value
       'True'. Note that the following Haskell functions do the basic
       operations on 'Bool'eans:

           (&&) :: Bool -> Bool -> Bool    -- 'AND'
           (||) :: Bool -> Bool -> Bool    -- 'OR'
           not  :: Bool -> Bool            -- 'NOT'
-}

eval_v1 :: Formula -> Bool
eval_v1 (Atom  s) = True
eval_v1 (And f g) = eval_v1 f && eval_v1 g
eval_v1 (Or  f g) = eval_v1 f || eval_v1 g
eval_v1 (Not f)   = not (eval_v1 f)


add :: Int -> Int -> Int
add = \x y -> x + y

{- (b) Now write a new version of 'eval_v1' that, instead of evaluating
       every 'Atom a' to 'True', takes a function that gives a 'Bool'
       for each atomic proposition: -}

eval :: (String -> Bool) -> Formula -> Bool
eval = \function formula -> case formula of
               (Atom  s) -> function s
               (And f g) -> (eval function f) && (eval function g)
               (Or  f g) -> (eval function f) || (eval function g)
               (Not f)   -> not (eval function f)


{- For example:

     eval (\s -> s == "A") (Or (Atom "A") (Atom "B"))  == True
     eval (\s -> s == "A") (And (Atom "A") (Atom "B")) == False
-}


{- 9. Substituting Formulas

   Write a function that, given a function 's' that turns 'String's
   into 'Formula's (a "substitution"), replaces all the atomic
   formulas in a Formula with whatever 'f' tells it to: -}

subst :: (String -> Formula) -> Formula -> Formula
subst = \function formula -> case formula of
               (Atom  s) -> function s
               (And f g) -> subst function f `And` subst function g
               (Or  f g) -> subst function f `Or`  subst function g
               (Not f)   -> Not (subst function f)

{- For example:

     subst (\s -> if s == "A" then Not (Atom "A") else Atom s) (And (Atom "A") (Atom "B")) == And (Not (Atom "A")) (Atom "B")
-}

{- 10. Composition

   Write a function '>>>' that composes two functions: takes two
   functions 'f' and 'g', and returns a function that first runs 'f'
   on its argument, and then runs 'g' on the result.

   HINT: this is similar to the function 'compose' above. -}

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = \f g a -> g (f a)

{- Try rewriting the 'numberOfEs' function from above using this one. -}



{- 11. Backwards application

   Write a function of the following type that takes a value 'x' and a
   function 'f' and applies 'f' to 'x'. Note that this functions takes
   its arguments in reverse order to normal function application! -}

(|>) :: a -> (a -> b) -> b
(|>) x f = f x 


{- This function can be used between its arguments like so:

       "HELLO" |> map toLower

   and it is useful for chaining calls left-to-right instead of
   right-to-left as is usual in Haskell:

       "EIEIO" |> onlyEs |> length
-}

{- 12. Flipping

   Write a function that takes a two argument function as an input,
   and returns a function that does the same thing, but takes its
   arguments in reverse order: -}

flip :: (a -> b -> c) -> b -> a -> c
flip  = \f a b -> f b a 
