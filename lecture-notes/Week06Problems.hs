module Week06Problems where

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- 1. Using 'Result' to handle errors.

   Here is the 'Result' type described in the notes. It is like the
   'Maybe' type except that the "fail" case has a String message
   attached: -}

data Result a
  = Ok a
  | Error String
  deriving (Eq, Show)

{- Implement 'returnOK', 'failure', 'ifOK' and 'catch' for 'Result'
   instead of 'Maybe'. Note that in 'failure' we have to provide an
   error message, and in 'catch' the "exception handler" gets the
   error message. -}

returnOk :: a -> Result a
returnOk = Ok

failure :: String -> Result a
failure = Error

--op is an operation that can be applied to the continuation, k, if a is
--exists. This is kind of like try {k r} catch (Error e)
ifOK :: Result a -> (a -> Result b) -> Result b
ifOK op k = case op of
         Error e -> Error e
         Ok    r -> k r

catch :: Result a -> (String -> Result a) -> Result a
catch (Ok a)      handler = Ok a
catch (Error msg) handler = handler msg

{- Reimplement 'search' to use 'Result' instead of 'Maybe'. We add 'Show
   k' to the requirements, so that we can put the key that wasn't
   found in the error message. -}

search :: (Show k, Eq k) => k -> [(k,v)] -> Result v
search k [] = Error ("key '"++show k++"' not present in list")
search k ((k', v'):kvs) =
   if k == k' then returnOk v'
   else search k kvs

{- Finally, reimplement 'lookupAll v4' to return 'Result (Tree v)'
   instead of 'Maybe (Tree v)'. (The code will be identical!) -}

lookupAll_v4 :: (Show k, Eq k) => [(k,v)] -> Tree k -> Result (Tree v)
lookupAll_v4 kvs Leaf = returnOk Leaf
lookupAll_v4 kvs (Node l v r) = 
   lookupAll_v4 kvs l `ifOK` \l' -> 
   search v kvs `ifOK` \v'       ->
   lookupAll_v4 kvs r `ifOK` \r' ->
      returnOk (Node l' v' r')


{- 2. Processes

   The following data type represents processes that can 'Input' lines
   and carry on given information about what that line is; 'Output'
   lines and then carry on being a process; or 'End', with a value. -}

data Process a
  = End a
  | Input (String -> Process a)
  | Output String (Process a)

{- Here is an example process, written out in full. It implements a
   simple interactive program: -}

interaction :: Process ()
interaction =
    Output "What is your name?"
    (Input (\name ->
      Output ("Hello " ++ name ++ "!")
     (Output "What is your name really?"
     (Input (\realName ->
      Output ("Okay, now I believe you "++realName) (End ()))))))

{- Processes by themselves do not do anything. They are only
   descriptions of what to do. To have an effect on the world, we to
   need to translate them to Haskell's primitives for doing I/O (we
   will cover this in more detail in Week 08): -}

runProcess :: Process a -> IO a
runProcess (End a)         = return a
runProcess (Input k)       = do line <- getLine; runProcess (k line)
runProcess (Output line p) = do putStrLn line; runProcess p

{- Now we can run the 'interaction' described above:

       > runProcess interaction
       What is your name?
       Bob                         <--- this line entered by the user
       Hello Bob!
-}

{- Writing out processes in the style of 'interaction' above is annoying
   due to the brackets needed. We can make it simpler by defining some
   functions, First we define two basic operations: 'input' and
   'output', which are little "mini-Processes" that do one input or
   output operation. -}

input :: Process String
input = Input (\x -> End x)

output :: String -> Process ()
output s = Output s (End ())

{- The key operation is sequencing of processes. First we (simulate) run
   one process, then we take the result value from that and use it to
   make a second process which we run. Note that this has the same
   flavour as the 'ifOK', 'andThen' and 'andThenWithPrinting'
   functions from the notes. -}

sequ :: Process a -> (a -> Process b) -> Process b
sequ (End a)      f = f a 
sequ (Input k)    f = Input (\x -> sequ (k x) f)
sequ (Output s p) f = Output s (sequ p f)

{- HINT: this is very very similar to the 'subst' function from Week 03.

   Once you have 'subst', you can define a neater version of
   'interaction' that makes the sequential nature clearer: -}

interaction_v2 :: Process ()
interaction_v2 =
  output "What is your name?"      `sequ` \() ->
  input                            `sequ` \name ->
  output ("Hello " ++ name ++ "!") `sequ` \() ->
  End ()

interaction_v3 :: Process ()
interaction_v3 = 
   output "How are you (say good)" `sequ` \()   ->
   input                          `sequ` \name ->
   if name == "good" then
      output "I am glad that you are good :)"
   else
      output "You are not good :("
   `sequ` \() -> 
   End ()

{- Running 'runProcess interaction_v2' should have the same effect as
   running 'runProcess interaction' did.

   Let's put sequ to work.

   Implement an interactive 'map' using 'input', 'output' and
   'sequ'. This is a 'map' that prompts the user for what string to
   use to replace each string in the input list. This will be similar
   to printAndSum_v2 from the notes.

   For example:

       > runProcess (interactiveMap ["A","B","C"])
       A
       a
       B
       b
       C
       c
       ["a","b","c"]

   where the lower case lines are entered by the user. -}

interactiveMap :: [String] -> Process [String]
interactiveMap [] = End []
interactiveMap (s:ss) = 
   output ("replace "++s++" with:") `sequ` \() -> 
   input    `sequ` \y ->
   interactiveMap ss `sequ` \ys -> 
   End (y:ys)

{- Finally, implement a function that does an 'interactive filter',
   similar to the interactive map. For every element in the input
   list, it outputs it and prompts for user input. If the user types
   "y" then the element is kept. Otherwise it is not copied into the
   output list. -}

interactiveFilter :: Show a => [a] -> Process [a]
interactiveFilter [] = End []
interactiveFilter (x:xs) = 
   output ("keep "++ show x++"?") `sequ` \() ->
   input                        `sequ` \char -> 
   if char == "Y" || char == "y" then
      interactiveFilter xs `sequ` \ys -> 
      End (x:ys)
   else
      interactiveFilter xs `sequ` \ys ->
      End ys

      
{- For example,

       > runProcess (interactiveFilter ["A","B","C"])
       Keep "A"?
       y
       Keep "B"?
       n
       Keep "C"?
       y
       ["A","C"]

-}
