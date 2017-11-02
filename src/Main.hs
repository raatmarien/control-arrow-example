-- module Main where

import qualified Control.Category as Cat
import Control.Arrow
import Data.Char

-- Writing Cat.. to compose Categories is a bit tedious, so lets use
-- <> for that (notice that it looks like the . operator a bit)
(<>) :: ProcessCounter b c -> ProcessCounter a b -> ProcessCounter a c
(<>) = (Cat..)

-- pid is just id for our ProcessCounter
pid :: ProcessCounter a a
pid = Cat.id

-- Lets call our custom Arrow ProcessCounter, because it will count
-- the amount of times a process is applied to the first argument to
-- get the answer.
-- A ProcessCounter from a to b is just a function with a hidden
-- counter.
newtype ProcessCounter a b = ProcessCounter {
  func :: (a, Int) -> (b, Int)
}

-- So lets make ProcessCounter an instance of Arrow, by implementing
-- arr and first.
instance Arrow ProcessCounter where
  -- Our arr function takes care of transforming a function to a
  -- ProcessCounter and here we hide the count increment.
  arr f = ProcessCounter (\(a, count) -> (f a, count + 1))
  first (ProcessCounter func) = ProcessCounter func'
    where
      func' ((a, b), count) = let (a', count') = func (a, count)
                              in ((a', b), count')

-- Our ProcessCounter also needs to be an instance of Category, in
-- order to be an instance of Arrow.
instance Cat.Category ProcessCounter where
  id = arr id
  (ProcessCounter f1) . (ProcessCounter f2) = ProcessCounter $ f1 . f2

-- Now lets create a function to actually run a ProcessCounter, this
-- takes care of setting the counter to zero at the start and
-- extracting the function from our ProcessCounter.
runProcessCounter :: ProcessCounter a b -> a -> (b, Int)
runProcessCounter p a = func p (a, 0)

-- Convenience function for easy printing
printProcess :: Show b => ProcessCounter a b -> a -> IO ()
printProcess p i = print $ runProcessCounter p i

-- To illustrate our example, lets make a process that will scream a
-- given string.
scream :: ProcessCounter String String
scream = arr $ (++ "!") . map toUpper

-- And a process that will greet a given string
greet :: ProcessCounter String String
greet = arr ("Hello, " ++)

-- Screaming "Haskell" and then greeting it, how many processes have
-- been applied?
applyingProcesses = printProcess (greet <> scream) "Haskell"

-- Applying pid 11 times, so we should receive a count of 11
applyingFromAList = printProcess (foldr (<>) pid (replicate 10 pid)) "Hey"

-- But because our ProcessCounter type is an Arrow, we can do a lot of
-- other things!

-- For example, remember the first function we defined, we can operate
-- on just a part of the input and all the while, it is remembering
-- the amount of processes.
applyingOnHalf = printProcess (first scream >>> arr (uncurry (++)))
                 ("hey", " you")

-- We can also combine processes.
-- But now we notice something interesting, if we look we count 3
-- processes applied to the input: (arr (splitAt 3)), (scream ***
-- greet) and (arr (uncurry (++))), but our ProcessCounter counted 6
-- processes!
-- The reason for this is the implementation of (***), the combine
-- operator. If we look at the source it is defined as this:
--     f *** g = first f >>> arr swap >>> first g >>> arr swap
--      where swap ~(x,y) = (y,x)
-- As you see, f ** g ammounts to 4 arrows applied after each other,
-- instead of one. So that means 4 processes instead of one, which
-- means that our counter is correct, there have been 6 processes
-- applied to this input.
-- We could redefine *** to change this, but lets keep it this way and
-- count the actual amount of processes under the hood.
combining = printProcess (arr (splitAt 3) >>> scream *** greet >>>
                          arr (uncurry (++))) "heyarrow"

-- Lastly, we have the fanout operator (&&&). This combines to arrows
-- with the same input and outputs both outputs! Lets put it to use.
-- Again, notice the counted amount of processes and see the
-- definition of (&&&) to understand why it is 5.
fanout = printProcess (scream &&& greet) "Haskell"

-- Our main function just prints all our examples!
main :: IO ()
main = sequence_ [applyingProcesses, applyingFromAList
                 , applyingOnHalf, combining, fanout]

