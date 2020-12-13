{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transformers where

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Morph
  ( MFunctor (hoist),
    MonadTrans (lift),
    generalize,
  )
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (..),
  )
import Control.Monad.State
  ( MonadState,
    State,
    StateT,
    evalState,
    gets,
    modify,
  )
import Control.Monad.Writer
  ( MonadWriter (tell),
    WriterT,
    execWriterT,
  )

-- lift :: (Monad m, MonadTrans t) => m a -> t m a
-- type State s = StateT s Identity
-- type Writer w = WriterT w Identity
-- type Reader r = ReaderT r Identity

-- | Monad Transformer Laws
-- lift . return = return
-- lift (m >>= f) = lift m >>= (lift . f)

-- | liftIO
-- class MonadTrans t where
--   lift :: Monad m => m a -> t m a

-- class (Monad m) => MonadIO m where
--   liftIO :: IO a -> m a

-- instance MonadIO IO where
--   liftIO = id

type Env = [(String, Int)]

type Eval a = ReaderT Env Maybe a

data Expr
  = Val Int
  | Add Expr Expr
  | Var String
  deriving (Show)

eval :: Expr -> Eval Int
eval ex = case ex of
  Val n -> return n
  Add x y -> do
    a <- eval x
    b <- eval y
    return (a + b)
  Var x -> do
    env <- ask
    lift (lookup x env)

env :: Env
env = [("x", 2), ("y", 5)]

ex1 :: Eval Int
ex1 = eval (Add (Val 2) (Add (Val 1) (Var "x")))

readerExample1, readerExample2 :: Maybe Int
readerExample1 = runReaderT ex1 env
readerExample2 = runReaderT ex1 []

-- | mtl
-- instance Monad m => MonadState s (StateT s m)
-- instance Monad m => MonadReader r (ReaderT r m)
-- instance (Monoid w, Monad m) => MonadWriter w (WriterT w m)
-- newtype Reader r a = Reader {runReader :: r -> a}

-- instance MonadReader r (Reader r) where
--   ask = Reader id
--   local f m = Reader (runReader m . f)

-- newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

-- instance (Monad m) => Monad (ReaderT r) where
--   return a = ReaderT $ \_ -> return a
--   m >>= k = ReaderT $ \r -> do
--     a <- runReaderT m r
--     runReaderT (k a) r

-- class (Monad m) => MonadReader r m | m -> r where
--   ask :: m r
--   local :: (r -> r) -> m a -> m a

-- instance (Monad m) => MonadReader r (ReaderT r m) where
--   ask = ReaderT return
--   local f m = ReaderT $ \r -> runReaderT m (f r)

newtype Quantity v a = Quantity a deriving (Eq, Ord, Num, Show)

data Haskeller

type Haskellers = Quantity Haskeller Int

a = Quantity 2 :: Haskellers

b = Quantity 6 :: Haskellers

totalHaskellers :: Haskellers
totalHaskellers = a + b

-- a little stack machine

type Stack = [Int]

type Output = [Int]

type Program = [Instr]

type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype Comp a = Comp {unComp :: VM a}
  deriving (Functor, Applicative, Monad, MonadReader Program, MonadWriter Output, MonadState Stack)

data Instr = Push Int | Pop | Puts

evalInstr :: Instr -> Comp ()
evalInstr instr = case instr of
  Pop -> modify tail
  Push n -> modify (n :)
  Puts -> do
    tos <- gets head
    tell [tos]

eval' :: Comp ()
eval' = do
  instr <- ask
  case instr of
    [] -> return ()
    (i : is) -> evalInstr i >> local (const is) eval'

execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp eval')

program :: Program
program = [Push 42, Push 27, Puts, Pop, Puts, Pop]

printProgram :: IO ()
printProgram = mapM_ print $ execVM program

newtype B = MkB Int

extractB :: B -> Int
extractB (MkB x) = x

-- | Monad Morphisms
-- mmporph
-- hoist :: Monad m => (forall a. m a -> n a) -> t m b -> t n b
-- generalize :: Monad m => Identity a -> m a
type Eval' a = State [Int] a

runEval :: [Int] -> Eval' a -> a
runEval = flip evalState

pop :: Eval' Int
pop = do
  top <- gets head
  modify tail
  return top

push :: Int -> Eval' ()
push x = modify (x :)

ev1 :: Eval' Int
ev1 = do
  push 3
  push 4
  pop
  pop

ev2 :: StateT [Int] IO ()
ev2 = do
  result <- hoist generalize ev1
  liftIO $ putStrLn $ "Result: " ++ show result

-- | Effect systems
-- Polysemy
-- Fused Effects