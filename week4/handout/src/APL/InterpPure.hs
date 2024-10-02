module APL.InterpPure (runEval) where



import APL.Monad


-- runEval :: EvalM a -> ([String], a)
runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s (k r)
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp str m)) = let (out, x) = runEval' r s m in (str : out, x)
    runEval' _ _ (Free (ErrorOp err)) = ([], Left err)


-- APL.InterpPure:
-- runEval :: EvalM a -> a
-- runEval = runEval' envEmpty stateInitial
--   where
--     runEval' :: Env -> State -> EvalM a -> a
--     runEval' _ _ (Pure x) = x
--     runEval' r s (Free (ReadOp k)) = runEval' r s (k r)
--     runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
--     runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m


-- APL.InterpPure:

{- 
data EvalOp a where
  ReadOp :: (Env -> a) -> EvalOp a

type EvalM a = Free EvalOp a
 
Explanation
Pattern Matching:

- The pattern Free (ReadOp k) matches a Free monadic value that wraps a ReadOp operation.
- ReadOp k indicates that the operation involves reading from the environment and then continuing with the function k.

Recursive Call:
- runEval' r (k r):
  - k r applies the continuation function k to the environment r, resulting in a new EvalM a
  - runEval' r (k r) then recursively evaluates this new computation with the same environment r.
-}