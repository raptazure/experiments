module Examples where

import Data.Array
import Prelude (bind, pure, (+), (==))
import Control.Plus (empty)

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n then
    pure [ x, y ]
  else
    empty

-- class
--   Apply m <= Bind m where
--   bind :: forall a b. m a -> (a -> m b) -> m b
-- class (Applicative m, Bind m) <= Monad m
-- instance bindArray :: Bind Array where
--   bind xs f = concatMap f xs
-- do value <- someComputation
--    whatToDoNext
-- bind someComputation \value -> whatToDoNext
-- someComputation >>= \value -> whatToDoNext
