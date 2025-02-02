{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week07Problems where

import Prelude hiding ( Monad (..)
                      , Applicative (..)
                      , mapM
                      , mapM_
                      , (<$>))
import Week07 hiding (search, lookupAll, ifThenElse, (>>))

{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. The 'Maybe' monad is useful for simulating exceptions. But when an
      exception is thrown, we don't get any information on what the
      exceptional condition was! The way to fix this is to use a type
      that includes some information on the 'Error' case: -}

data Result a
  = Ok a
  | Error String
  deriving Show

{-    Write a Monad instance for 'Result', using the code from your
      'returnOk' and 'ifOK' functions from last week, and then use it
      to rewrite the 'search' and 'lookupAll' functions. -}

instance Monad Result where
  return :: a -> Result a 
  return a = Ok a 

  (>>=) :: Result a -> (a -> Result b) -> Result b
  op >>= f = case op of 
            Error e -> Error e 
            Ok a    -> f a

search :: (Eq k, Show k) => k -> [(k, v)] -> Result v
search k [] =  Error ("Key "++ show k ++" not present")
search k ((k',v'):kvs) =
   if k == k' then
      return v'
    else
      search k kvs 

lookupAll :: (Eq k, Show k) => [(k,v)] -> Tree k -> Result (Tree v)
lookupAll kvs Leaf = Ok Leaf
lookupAll kvs (Node l v r) = 
  do 
    l' <- lookupAll kvs l
    v' <- search v kvs
    r' <- lookupAll kvs r
    return (Node l' v' r')

{- 2. Write a function using the Printing monad and 'do' notation that
      "prints out" all the strings in a tree of 'String's: -}

printTree :: Tree String -> Printing ()
printTree Leaf = return () 
printTree (Node l v r) =
      do    printTree l
            printLine v 
            printTree r 

{- 3. The implementation of 'sumImp' in the notes can only sum up lists
      of 'Int's.

      (a) What changes would you have to make to 'State' so that you
          can add up lists of 'Double's? You'll have to make a new
          newtype like 'State', and reimplement the 'runState', the
          'Monad' instance, the 'get' and 'put' function, and finally
          the 'sumpImp' function. The changes to the actual code will
          be minimal, if anything. All the changes are in the types. -}

newtype StateDouble a = MkStateDouble (Double -> (Double, a))

runStateDouble :: StateDouble a -> Double -> (Double, a)
runStateDouble (MkStateDouble t) = t

instance Monad StateDouble where 
  return :: a -> StateDouble a 
  return x = MkStateDouble (\s -> (s, x))

  (>>=) :: StateDouble a -> (a -> StateDouble b) -> StateDouble b
  op >>= f =
     MkStateDouble (\s -> 
       let (s0, a) = runStateDouble op s
           (s1, b) = runStateDouble (f a) s0
           in (s1, b))

getDouble :: StateDouble Double
getDouble = MkStateDouble (\s -> (s,s))

putDouble :: Double -> StateDouble ()
putDouble i = MkStateDouble (\_ -> (i,()))

sumImpDouble :: [Double] -> StateDouble Double 
sumImpDouble xs = 
  do 
    putDouble 0
    for_ xs (\x -> 
      do 
        total <- getDouble
        putDouble (total + x))
    getDouble
        
{-    (b) Make an alternative version of 'State' that is parameterised
          by the type of the state (so that someone using it can
          decide whether it is 'Int' or 'Double' for instance). -}


newtype StateGeneric s a = MkStateGeneric (s -> (s,a))

runStateGeneric :: StateGeneric s a -> s -> (s,a)
runStateGeneric (MkStateGeneric a) = a

instance Monad (StateGeneric s) where
  return :: a -> StateGeneric s a
  return a = MkStateGeneric (\s -> (s,a))

  (>>=) :: StateGeneric s a -> (a -> StateGeneric s b) -> StateGeneric s b
  op >>= f = 
    MkStateGeneric (\s ->
      let (s0, a) = runStateGeneric op s
          (s1, b) = runStateGeneric (f a) s0
      in (s1, b))

getGeneric :: StateGeneric s s
getGeneric = MkStateGeneric (\s -> (s,s)) 

putGeneric :: s -> StateGeneric s () 
putGeneric s = MkStateGeneric (\_ -> (s,()))

sumImpGeneric :: Monoid m => [m] -> StateGeneric m m
sumImpGeneric xs = 
  do 
    putGeneric mempty
    for_ xs (\x -> 
      do 
      total <- getGeneric
      putGeneric (total <> x))
    getGeneric

{- 4. Write a function like mapM that works on 'Tree's instead of lists: -}

mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f Leaf = return Leaf
mapTreeM f (Node l v r) =
  do 
    l' <- mapTreeM f l
    v' <- f v
    r' <- mapTreeM f r
    return (Node l' v' r')

{- 5. Write a function like mapM that works on 'Maybe's instead of lists: -}

mapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMaybeM f Nothing = return Nothing 
mapMaybeM f (Just x) = 
  -- f x >>= \y -> return (Just y)    the "raw way"
  do
    y <- f x
    return (Just y)
