{-# LANGUAGE InstanceSigs #-}
module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env


localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x
  (<*>) = ap

instance Monad EvalM where
  (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
         in y env

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

runEval :: EvalM a -> Either Error a
-- runEval = undefined -- TODO
runEval (EvalM m) = m envEmpty

-- evalIntBinOp :: Exp -> Exp -> (Integer -> Integer -> EvalM Integer) -> EvalM Val
-- evalIntBinOp e1 e2 op = do
--   x <- eval e1
--   y <- eval e2
--   case (x, y) of
--     (ValInt i, ValInt j) -> do
--       result <- op i j
--       pure $ ValInt result
--     _ -> failure "Non-integer operand"

evalIntBinOp :: Exp -> Exp -> (Integer -> Integer -> EvalM Integer) -> EvalM Val
evalIntBinOp e1 e2 op =
  eval e1 >>= \x ->
  eval e2 >>= \y ->
  case (x, y) of
    (ValInt i, ValInt j) -> 
      op i j >>= \result ->
      pure $ ValInt result
    _ -> failure "Non-integer operand"


-- catch
catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x


eval :: Exp -> EvalM Val-- eval = undefined -- TODO
eval (CstInt i) = pure $ ValInt i
eval (CstBool b) = pure $ ValBool b
-- eval (Add e1 e2) = evalIntBinOp env e1 e2 (+)
eval (Add e1 e2) = evalIntBinOp e1 e2 (\x y -> pure (x + y))
eval (Sub e1 e2) = evalIntBinOp e1 e2 (\x y -> pure (x - y))
eval (Mul e1 e2) = evalIntBinOp e1 e2 (\x y -> pure (x * y))
eval (Pow e1 e2) = evalIntBinOp e1 e2 checkedPow
  where
    checkedPow x y =
      if y < 0 then failure "Negative exponent"
      else pure $ x ^ y

eval (Div e1 e2) = evalIntBinOp e1 e2 checkedDiv
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv v1 v2 = pure $ v1 `div` v2

eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable " ++ v

eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    _ -> failure "Invalid operands to equality"

eval (If cond e1 e2) = do
  c <- eval cond
  case c of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Condition must be a boolean"

eval (Let var e1 e2) = do
  v <- eval e1
  localEnv (envExtend var v) (eval e2)

eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body

eval (Apply e1 e2) = do
  f <- eval e1
  a <- eval e2
  case (f, a) of
    (ValFun fenv var body, arg) ->
      -- let newEnv = envExtend var a fenv
      -- eval  body
      localEnv (\_ -> envExtend var arg fenv) $ eval body
    _ -> failure "Can only apply functions"

eval (TryCatch e1 e2) = do
  catch (eval e1) (eval e2)

-- eval (KvGet v) = do
  