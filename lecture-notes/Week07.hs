{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week07 where

import Prelude hiding ( Monad (..)
                      , Applicative (..)
                      , mapM
                      , mapM_
                      , (<$>))
import Data.Char      (isDigit, digitToInt)

{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y


{-    WEEK 7 : MONADS

   Last week we saw three examples of how to simulate side effects
   with "pure" code in Haskell:

     1. simulating exceptions using the 'Maybe' type,

     2. simulating mutable state by explicit state passing, and

     3. simulating printing by collecting outputs.

   This week, we look at the common pattern in all these examples, and
   give it a name: 'Monad'. -}


{-    Part 7.1 : DEFINING MONADS and THE MAYBE MONAD

   In each of the three cases, we saw that there is common
   structure. Each one had a "do nothing" operation:

       returnOk       :: a -> Maybe a
       returnState    :: a -> State a
       returnPrinting :: a -> Printing a

   and a "do this, then do that" operation:

       ifOK                :: Maybe a ->    (a -> Maybe b)    -> Maybe b
       andThen             :: State a ->    (a -> State b)    -> State b
       andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b

   The Week 06 tutorial questions asked you to write this function for
   'Process'es, with yet again a similar type.

       sequ                :: Process a ->  (a -> Process b)  -> Process b

   If there was only one example, it wouldn't be interesting. Two or
   three is maybe a coincidence. But four examples is calling out for
   this common pattern to be given a name.

   For historical reasons coming from the mathematical field of
   Category Theory, that name is "Monad", which is not the best name
   that could have been used in a programming context. However, it is
   the name that has stuck, so we use it.

      ASIDE: One of the inventors of Haskell, and the main author of
      the GHC compiler, Simon Peyton Jones, has suggested that the
      name 'Monad' has been quite harmful, and that they should have
      been called 'Warm Fuzzy Thing' instead. This link is to slides
      from a talk entitled "Wearing the hair shirt: a retrospective on
      Haskell" from 2003, where he looks back on the first 15 years of
      Haskell as it was then:

          https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/HaskellRetrospective.ppt



   Along with the name 'Monad', we have two standard names for the "do
   nothing" and "do this, then that" operations. The first is called
   'return'. The second is called '>>=', which is pronounced 'bind'
   because we think of it "binding" the result of the first operation
   in the continuation.

   In Haskell, we can use a type class to give a name to all type
   constructors 'm' that have a 'returnOk' and a '(>>=)': -}

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

{- This type class definition answers the question "what is a 'Monad'?"
   A Monad is any type constructor 'm' (e.g., 'Maybe', 'State',
   'Printing', 'Process') that has two operations: 'return' and
   '(>>=)' with the types shown. Of course, this doesn't answer the
   question of why this is a useful definition. Hopefully, the
   examples we have seen so far will have gone some way to justifying
   the cost of introducing a new type class in terms of having to
   understand new things.

   Let's now see how to make the examples from last week into
   instances of the 'Monad' type class. Each of these will also have
   several extra operations beyond the 'return' and '>>=' that are
   peculiar to that instance.

   We'll also see that Haskell treats the 'Monad' type class a little
   bit specially, in that Haskell has special syntax for writing
   programs that use '>>=' ("bind"), called "'do' notation". -}


{- In Week 06, we saw definitions of 'returnOk' and 'ifOK' for
   'Maybe'. Let's now put them in an instance for the 'Monad' type
   class, declaring 'Maybe' to be a 'Monad' by implementing the two
   required functions: -}

instance Monad Maybe where
  return :: a -> Maybe a
  return x = Just x

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  op >>= f = case op of
               Nothing -> Nothing
               Just x  -> f x

{- The 'Maybe' monad also has a special operation called 'failure', for
   representing an exception being thrown: -}

failure :: Maybe a
failure = Nothing

{- And a 'catch' operation that simulates an exception being caught: -}

catch :: Maybe a -> Maybe a -> Maybe a
catch op handler =
  case op of
    Nothing -> handler
    Just x  -> Just x


{-    Part 7.2 : 'do' NOTATION

   Last week, we saw that using functions like 'ifOK', 'andThen' and
   'andThenWithPrinting', we can significantly tidy up functions that
   perform side effects. However, they are still a little bit messy
   due to the repeated use of the helper functions. Now that we've
   defined the 'Monad' type class, we do at least have the option of
   using the same function name ('>>=') every time. However, Haskell
   also provides a handy notation for making functions that use Monads
   look nicer.

   Let's look at the 'lookupAll' function from Week 06 and see how
   'do' makes things a little simpler. Here's the 'Tree' datatype: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- And the 'search' function that may fail to find a 'k'ey: -}

search :: Eq k => k -> [(k,v)] -> Maybe v
search k []            = failure
search k ((k',v'):kvs) = if k == k' then return v' else search k kvs

lookupAll :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll kvs Leaf =
  return Leaf
lookupAll kvs (Node l k r) =
  lookupAll kvs l  >>= \l' ->
  search k kvs     >>= \v ->
  lookupAll kvs r  >>= \r' ->
  return (Node l' v r')

{- This definition is still a bit messy, since it involves a lot of
   repetition of '>>=' and uses of '\' (lambda) to write
   functions. Also, the names for the results of operations is to the
   right of the operation that produces them, which is a bit weird. We
   can use Haskell's "'do' notation" to make this look nicer: -}

lookupAll_v2 :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll_v2 kvs Leaf =
  return Leaf
lookupAll_v2 kvs (Node l k r) =
  do l' <- lookupAll_v2 kvs l
     v  <- search k kvs
     r' <- lookupAll_v2 kvs r
     return (Node l' v r')

{- Which hides all the uses of '>>=' and the lambdas, and starts to look
   like a normal sequence of instructions to be executed one after the
   other.

   How does 'do' notation work? The Haskell compiler translates each
   line as follows:

     do x <- e1     becomes    e1 >>= (\x -> e2)
        e2

   and

     do e1          becomes    e1 >> e2
        e2

   (where '(>>) op1 op2 = op >>= \_ -> op2')

   and

     do e           becomes    e

   This kind of translation is sometimes called "syntactic sugar": it
   adds nothing to the language that we couldn't do before, but
   sweetens the experience a bit.

      "Syntactic sugar causes cancer of the semicolon"
        -- Alan J. Perlis
           Epigrams in Programming
           http://www.cs.yale.edu/homes/perlis-alan/quotes.html

-}


{-    Part 7.3 : STATE MONAD

   In Week 06, we defined a type synonym for "state mutating operation
   that returns a value of type 'a'":

       type State a = Int -> (Int, a)

   Haskell's type class feature doesn't allow us to define type
   synonyms as instances of type classes, so we need to define a
   'newtype' that does the same thing, but this time with a
   constructor: -}

newtype State a = MkState (Int -> (Int, a))

{- The 'runState' function now pattern matches on the 'MkState'
   constructor and returns the underlying state mutation function: -}

runState :: State a -> Int -> (Int, a)
runState (MkState t) = t

{- Now we can define the 'Monad' instance for 'State', using the same
   definitions as in Week 06, except with extra uses of 'MkState' and
   'runState' to move between the 'State' type and the underlying
   representation: -}

instance Monad State where
  return :: a -> State a
  return x =
    MkState (\s -> (s, x))

  (>>=) :: State a -> (a -> State b) -> State b
  op >>= f =
    MkState (\s ->
               let (s0, a) = runState op s
                   (s1, b) = runState (f a) s0
               in (s1, b))

{- The 'get' and 'put' primitive state mutators are defined as before,
   again with an extra 'MkState': -}

get :: State Int
get = MkState (\s -> (s,s))

put :: Int -> State ()
put i = MkState (\_ -> (i,()))

{- Because we have defined a 'Monad' instance for 'State', we
   automatically get to use 'do' notation. For example, here is the
   'getAndIncrement' state mutation operation from Week 06, written in
   as a sequence of steps: -}

getAndIncrement :: State Int
getAndIncrement =
  do x <- get
     put (x+1)
     return x

{- Now we can write 'numberTree' using 'do' notation, which makes the
   whole implementation easier to read: -}

numberTree :: Tree a -> State (Tree (a, Int))
numberTree Leaf =
  return Leaf
numberTree (Node l x r) =
  do l' <- numberTree l
     i  <- getAndIncrement
     r' <- numberTree r
     return (Node l' (x, i) r')


{-    Part 7.4 PRINTING MONAD -}

{- Just as we made 'State' an instance of 'Monad', we can do the same
   for 'Printing'. In the Week 06, we defined 'Printing' as a type
   synonym:

      type Printing a = ([String]), a)

   Again, due to the way that type classes work, we need to define a
   new datatype. Here, this datatype has one constructor which takes
   two arguments: the list of strings that have been printed, and the
   result value: -}

data Printing a = MkPrinting [String] a
  deriving Show

{- The definitions of 'return' and '>>=' are the same as in Week 06,
   except with the uses of 'MkPrinting' instead of the pair type '( ,
   )': -}

instance Monad Printing where
  return :: a -> Printing a
  return x = MkPrinting [] x

  (>>=) :: Printing a -> (a -> Printing b) -> Printing b
  op >>= f =
    let MkPrinting o1 a = op
        MkPrinting o2 b = f a
    in MkPrinting (o1 ++ o2) b

{- The primitive operation for 'Printing' is 'printLine', as we wrote in
   Week 06: -}

printLine :: String -> Printing ()
printLine s = MkPrinting [s] ()

{- Again, because 'Printing' is a 'Monad', we can use the 'do' notation
   with it. Here is an 'add' function that adds its arguments, but
   also emits a logging message as it does so. The type signature
   tells us that it may do some 'Printing': -}

add :: Int -> Int -> Printing Int
add x y =
  do printLine ("Adding " ++ show x ++ " and " ++ show y)
     return (x+y)

{- We can also rewrite the 'printAndSum' function from Week 06: -}

printAndSum :: Tree Int -> Printing Int
printAndSum Leaf =
  return 0
printAndSum (Node l x r) =
  do lsum <- printAndSum l
     printLine (show x)
     rsum <- printAndSum r
     return (lsum + x + rsum)


{-    Part 7.5 : FUNCTIONS FOR ALL MONADS

   We've already seen one benefit of declaring the 'Monad' type class:
   we can use 'do' notation to simplify programs that are best written
   as a sequence of steps.

   Another advantage that we'll come back to in the next few weeks is
   the ability to define Monads that don't have analogues in most
   languages. For instance, we can define a Monad of "multi-valued
   functions", which is useful for defining search functions, and a
   Monad of "parsers".

   A third advantage is the ability to write functions that work for
   all monads, not just 'Maybe', 'State', 'Printing', etc. This allows
   us to capture common patterns, such as mapping a function over a
   list, while doing some side effects. This function is called
   'mapM': -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) =
  do y  <- f x
     ys <- mapM f xs
     return (y:ys)

{- From the type signature, we can see that 'mapM' is similar to 'map':

      map :: (a -> b) -> [a] -> [b]

   except that the function argument has type 'a -> m b', and the
   final '[b]' has an 'm' before it. These extra 'm's indicate that
   the function passed to 'mapM' may perform some side effects for
   every 'a' in the input list, and that some side effects may be
   performed before generating the output list.

   An example usage of 'mapM' is the following. Let's say we have a
   list of characters, which we expect to be digits '0' .. '9', but
   we're not sure if they all are. We want to convert the list into a
   list of 'Int's corresponding to the digits, but raise an exception
   if we find a non-digit.

   We can use 'Maybe' to simulate raising an exception, and write this
   function as follows, using two functions from the 'Data.Char'
   module in the standard library: -}

readDigits :: [Char] -> Maybe [Int]
readDigits = mapM (\c -> if isDigit c then
                           return (digitToInt c)
                         else
                           failure)

{- Often we just want to perform some side effect for every element in
   the list, and we don't care about generating an output list. In
   that case, we can use a variant of 'mapM' that doesn't build an
   output list: -}

mapM_ :: Monad m => (a -> m ()) -> [a] -> m ()
mapM_ f [] = return ()
mapM_ f (x:xs) =
  do f x
     mapM_ f xs

{- The underscore at the end of the name is a convention for a variant
   of a function that returns '()' instead of another data structure.

   'mapM_' enables us to write something like the 'for each' loops in
   languages that have pervasive side effects. For instance, to print
   out each element of a list using the 'Printing' monad, we use
   'mapM_' to iterate over the list, instead of doing the recursion
   ourselves: -}

printList :: Show a => [a] -> Printing ()
printList = mapM_ (\x -> printLine (show x))

{- To make this look a bit nicer, the standard library defines a version
   of 'mapM_' called 'for_' tha swaps the order of the arguments: -}

for_ :: Monad m => [a] -> (a -> m ()) -> m ()
for_ xs f = mapM_ f xs

{- Swapping the order of the arguments enables us to write things like:

     for_ <list of things> (\x -> <thing to do to 'x'>)

   Like printing out the whole list: -}

printList_v2 :: Show a => [a] -> Printing ()
printList_v2 xs =
  for_ xs (\x -> printLine (show x))

{- Or printing out the numbers from 1 to 10 (using the [1..10]
   notation): -}

printNumbers :: Printing ()
printNumbers =
  for_ [1..10] (\i -> printLine (show i))

{- Using the 'State' monad, we can write functions that we previous
   wrote using recursion and immutable variables using loops and
   mutable variables. For example, getting the length of a list by
   starting a counter at 0, and then adding 1 to the counter for each
   element of the list: -}

lengthImp :: [a] -> State Int
lengthImp xs =
  do put 0
     for_ xs (\_ -> do
       len <- get
       put (len+1))
     get


{- Or summing up a list by using the mutable state as the running total: -}

sumImp :: [Int] -> State Int
sumImp xs =
  do put 0
     for_ xs (\x -> do
       total <- get
       put (total + x))
     get
