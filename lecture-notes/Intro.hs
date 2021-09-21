module Intro where
import GHC.Generics (SourceUnpackedness)

{-    WELCOME TO

        CS316

          FUNCTIONAL PROGRAMMING
-}

















{- In this course, you will:

     - Learn more about Functional Programming (in Haskell)



   (Typed) Functional Programming is

     - Defining Datatypes To Represent Problems

     - Defining Functions To Create New Data From Old

   a.k.a "Value-oriented" programming.

   A "Functional Programming Language" is a programming language that
   is designed to make it easy to use Functional Programming ideas. -}









{- We use Haskell as an example Functional Programming Language.

     - Many languages now include ideas originally from Functional Programming.

        - Functions as values (a.k.a "lambdas")

        - Immutability

        - Expressive Types

        - Errors as data, instead of Exceptions

        - No 'null' (the "Billion dollar mistake")

        - Close tracking of possible "side effects"

   Haskell is not perfect (I will grumble about it during the course
   [*]), but it does offer a place to learn about Functional
   Programming concepts without too many distractions.

   [*] "There are only two kinds of languages: the ones people
       complain about and the ones nobody uses.”  ― Bjarne Stroustrup,
       The C++ Programming Language
-}


{- Course arrangements:

   - All online

   - Video lectures will "live" coding

   - Online lecture notes in a GitHub repository

   - Tutorial sessions to go through tutorial questions

-}














{- YOU WILL NEED A WORKING HASKELL INSTALLATION

   - Suggested setup:

       - Stack + VSCode + Haskell extension

       - I will use Emacs in the videos.

   - There are instructions on MyPlace

   - I (unfortunately) cannot test on Windows, so I will need the
     class's help to iron out Windows problems.

-}







{- Assessment:

   - One class test

   - One large coursework
-}

data Suit 
   = Heart
   | Club
   | Spade
   | Diamond
   deriving Show

data Colour
   = Red | Black
   deriving Show

data Value
   = Ace | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | Jack | Queen | King
   deriving Show 

colourOfSuit :: Suit -> Colour
colourOfSuit Heart   = Red 
colourOfSuit Diamond = Red
colourOfSuit _       = Black

numericValue :: Value -> Integer
numericValue Ace   = 1
numericValue N2    = 2 
numericValue N3    = 3
numericValue N4    = 4
numericValue N5    = 5 
numericValue N6    = 6 
numericValue N7    = 7
numericValue N8    = 8 
numericValue N9    = 9 
numericValue N10   = 10
numericValue Jack  = 11
numericValue Queen = 12 
numericValue King  = 13 

greaterValue :: Value -> Value -> Bool
greaterValue a b = valA > valB
   where valA = numericValue a
         valB = numericValue b 

data Card 
   = Card Suit Value
   deriving Show

getSuit :: Card -> Suit
getSuit (Card suit value) = suit

getColour :: Card -> Colour
getColour (Card suit value) = colourOfSuit suit

getValue :: Card -> Value
getValue (Card suit value) = value

getNumericValue :: Card -> Integer
getNumericValue (Card suit value) = numericValue value 