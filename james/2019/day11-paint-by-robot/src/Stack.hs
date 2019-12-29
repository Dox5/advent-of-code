module Stack where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State

newtype Stack a = Stack [a]
  deriving (Show)

empty :: Stack a
empty = Stack []

pop :: State (Stack a) (Maybe a)
pop = do
  (Stack s) <- State.get
  case s of
    (h:s') -> do
      State.put (Stack s')
      return $ Just h
    [] -> return Nothing

peek :: State (Stack a) (Maybe a)
peek = do
  (Stack s) <- State.get
  case s of
    (h:_) -> return (Just h)
    (_) -> return Nothing

push :: a -> State (Stack a) ()
push x = State.modify (\(Stack v) -> Stack (x:v))
