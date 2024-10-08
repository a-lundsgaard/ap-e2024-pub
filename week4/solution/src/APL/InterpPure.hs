module APL.InterpPure (runEval) where

import APL.Monad
  ( Env,
    Error,
    EvalM,
    EvalOp
      ( ErrorOp,
        KvGetOp,
        KvPutOp,
        PrintOp,
        ReadOp,
        StateGetOp,
        StatePutOp,
        TryCatchOp
      ),
    Free (Free, Pure),
    State,
    envEmpty,
    stateInitial,
  )

runEval :: EvalM a -> ([String], Either Error a)
runEval em = runEval' envEmpty stateInitial em
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' env s (Free (TryCatchOp e1 e2)) = case runEval' env s e1 of
      (_, Left _) -> runEval' env s e2
      (ps, Right x) -> (ps, Right x)
    runEval' env s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEval' env s $ k val
        Nothing -> ([], Left $ "Key not found: " ++ show key)
    runEval' env s (Free (KvPutOp key val m)) =
      -- filter out the key if it exists and replace it with the new value
      let s' = (key, val) : filter (\(k, _) -> k /= key) s
      in runEval' env s' m
