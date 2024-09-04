module APL.Eval
  ( Val(..)
  , eval
  , envEmpty
  , envExtend
  , envLookup
  )
where

import APL.AST (Exp(..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Env = [(VName, Val)]
type Error = String

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend name val env = (name, val) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

-- | Evaluate an expression in the given environment.
eval :: Env -> Exp -> Either Error Val
eval _ (CstInt n) = Right $ ValInt n
eval _ (CstBool b) = Right $ ValBool b
eval env (Add e1 e2) = evalBinOp env e1 e2 AddOp
eval env (Sub e1 e2) = evalBinOp env e1 e2 SubOp
eval env (Mul e1 e2) = evalBinOp env e1 e2 MulOp
eval env (Div e1 e2) = 
  case (eval env e1, eval env e2) of
    (Right (ValInt _), Right (ValInt 0)) -> Left "Division by zero"
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x `div` y
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left "Invalid operands to division"
eval env (Pow e1 e2) =
  case (eval env e1, eval env e2) of
    (Right (ValInt x), Right (ValInt y)) ->
      if y < 0 then
        Left "Negative exponent"
      else
        Right $ ValInt $ x ^ y
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left "Invalid operands to exponentiation"
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (Var name) =
  case envLookup name env of
    Nothing -> Left $ "Variable " ++ name ++ " not found"
    Just val -> Right val
eval env (Let name e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right val -> eval (envExtend name val env) e2
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Condition in if expression must be a boolean"


-- Helper function to evaluate binary operations
data BinOp = AddOp | SubOp | MulOp
evalBinOp :: Env -> Exp -> Exp -> BinOp -> Either Error Val
evalBinOp env e1 e2 op =
  case (eval env e1, eval env e2) of
    (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (applyOp op x y))
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left "Invalid operands to binary operation"

applyOp :: BinOp -> Integer -> Integer -> Integer
applyOp AddOp = (+)
applyOp SubOp = (-)
applyOp MulOp = (*)