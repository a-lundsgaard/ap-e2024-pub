module APL.InterpIO (runEvalIO) where

import APL.Monad

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO e = runEvalIO' envEmpty stateInitial e
  where
    runEvalIO' :: Env -> State -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r s (Free (ReadOp k)) = runEvalIO' r s $ k r 
    runEvalIO' r s (Free (StateGetOp k)) = runEvalIO' r s $ k s
    runEvalIO' r _ (Free (StatePutOp s' m)) = runEvalIO' r s' m
    runEvalIO' r s (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r s m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalIO' env s (Free (TryCatchOp e1 e2)) = do
      res <- runEvalIO' env s e1
      case res of
        Left _ -> runEvalIO' env s e2
        Right x -> pure $ Right x
    -- runEvalIO' r s (Free (KvGetOp k v)) = runEvalIO' r s (v k)
    -- runEvalIO' r s (Free (KvPutOp _ _ l)) = runEvalIO' r s l
    runEvalIO' env s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEvalIO' env s $ k val
        Nothing -> pure $ Left $ "Key not found: " ++ show key
    runEvalIO' env s (Free (KvPutOp key val m)) =
      -- filter out the key if it exists and replace it with the new value
      let s' = (key, val) : filter (\(k, _) -> k /= key) s
      in runEvalIO' env s' m