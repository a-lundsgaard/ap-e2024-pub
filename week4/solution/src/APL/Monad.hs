{-# LANGUAGE InstanceSigs #-}
module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    getState,
    putState,
    modifyState,
    evalKvGet,
    evalKvPut,
    evalPrint,
    failure,
    catch,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) :: Functor e => Free e (a -> b) -> Free e a -> Free e b
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  (>>=) :: Functor e => Free e a -> (a -> Free e b) -> Free e b
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
  | PrintOp String a
  | ErrorOp Error
  | TryCatchOp a a
  | KvGetOp Val (Val -> a)
  | KvPutOp Val Val a

instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGetOp k) = StateGetOp $ f . k
  fmap f (StatePutOp s m) = StatePutOp s $ f m
  fmap f (PrintOp p m) = PrintOp p $ f m
  fmap _ (ErrorOp e) = ErrorOp e
  fmap f (TryCatchOp m h) = TryCatchOp (f m) (f h)

  -- fmap f (KvGetOp v k) = KvGetOp v $ f . k
  fmap f (KvGetOp v k) = KvGetOp v (\x -> f (k x))
  fmap f (KvPutOp v1 v2 m) = KvPutOp v1 v2 $ f m

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op

getState :: EvalM State
getState = Free $ StateGetOp $ \s -> pure s

putState :: State -> EvalM ()
putState s = Free $ StatePutOp s $ pure ()

modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  s <- getState
  putState $ f s

evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure ()

failure :: String -> EvalM a
failure = Free . ErrorOp

catch :: EvalM a -> EvalM a -> EvalM a
catch m h = Free $ TryCatchOp m h

evalKvGet :: Val -> EvalM Val
evalKvGet v = Free $ KvGetOp v pure

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = Free $ KvPutOp k v (pure ())  
