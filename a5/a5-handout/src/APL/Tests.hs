module APL.Tests
  ( properties,
  )
where

import APL.AST (Exp (..), VName, subExp, printExp)
import APL.Check (checkExp)
import APL.Error (isDomainError, isTypeError, isVariableError)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Property,
    checkCoverage,
    cover,
    property,
  )
import Test.QuickCheck.Gen
import APL.Eval (runEval)
import APL.Parser (parseAPL)
import Debug.Trace (trace)

instance Arbitrary Exp where
  arbitrary = sized (\size -> genExp size [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

-- genExp :: Int -> Gen Exp
-- genExp 0 = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
-- genExp size =
--   oneof
--     [ CstInt <$> arbitrary
--     , CstBool <$> arbitrary
--     , Add <$> genExp halfSize <*> genExp halfSize
--     , Sub <$> genExp halfSize <*> genExp halfSize
--     , Mul <$> genExp halfSize <*> genExp halfSize
--     , Div <$> genExp halfSize <*> genExp halfSize
--     , Pow <$> genExp halfSize <*> genExp halfSize
--     , Eql <$> genExp halfSize <*> genExp halfSize
--     , If <$> genExp thirdSize <*> genExp thirdSize <*> genExp thirdSize
--     , Var <$> arbitrary
--     , Let <$> arbitrary <*> genExp halfSize <*> genExp halfSize
--     , Lambda <$> arbitrary <*> genExp (size - 1)
--     , Apply <$> genExp halfSize <*> genExp halfSize
--     , TryCatch <$> genExp halfSize <*> genExp halfSize
--     ]
--   where
--     halfSize = size `div` 2
--     thirdSize = size `div` 3

-- Generate expressions with controlled frequency
genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = frequency [(1, CstInt <$> arbitrary), (1, CstBool <$> arbitrary)]
genExp size vars = do
  newVar <- genVar
  frequency
    [ (10, CstInt <$> arbitrary),
      (10, CstBool <$> arbitrary),
      (15, Add <$> genExp halfSize vars <*> genExp halfSize vars),
      (15, Sub <$> genExp halfSize vars <*> genExp halfSize vars),
      (15, Mul <$> genExp halfSize vars <*> genExp halfSize vars),
      (10, genDiv halfSize vars), -- for generating division by zero
      (10, Pow <$> genExp halfSize vars <*> genExp halfSize vars),
      (10, Eql <$> genExp halfSize vars <*> genExp halfSize vars),
      (10, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars),
      (10, Let newVar <$> genExp halfSize vars <*> genExp halfSize (newVar : vars)), -- Ensure newVar matches genVar
      (10, Lambda newVar <$> genExp (size - 1) (newVar : vars)), -- Ensure newVar matches genVar
      (10, Apply <$> genExp halfSize vars <*> genExp halfSize vars),
      (10, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars),
      (if null vars then 0 else 10, Var <$> elements vars), -- Ensure vars are used
      (3, pure (Var newVar)) -- Introduce potential variable errors
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

genVar :: Gen VName
genVar = do
  len <- choose (2, 4)
  vectorOf len (elements ['a' .. 'z'])

-- genVar :: [VName] -> Gen VName
-- genVar vars = do
--   let probOfGenNewVar = 50
--   if null vars
--     then genNewVar vars
--     else frequency [(probOfGenNewVar, genNewVar vars), (100 - probOfGenNewVar, elements vars)]

-- genNewVar :: [VName] -> Gen VName
-- genNewVar vars = do
--   len <- choose (2, 4)
--   newVar <- vectorOf len (elements ['a' .. 'z'])
--   if newVar `elem` vars
--     then genNewVar vars
--     else return newVar

genDiv :: Int -> [VName] -> Gen Exp
genDiv size vars = do
  let probOfErr = 20
  numerator <- genExp (size `div` 2) vars
  denominator <- frequency [(probOfErr, return $ CstInt 0), (100 - probOfErr, genNonZeroExp (size `div` 2) vars)]
  return $ Div numerator denominator

genNonZeroExp :: Int -> [VName] -> Gen Exp
genNonZeroExp size vars = genExp size vars `suchThat` isNonZero

isNonZero :: Exp -> Bool
isNonZero (CstInt 0) = False
isNonZero _ = True

expCoverage :: Exp -> Property
expCoverage e =
  checkCoverage
    . cover 20 (any isDomainError (checkExp e)) "domain error"
    . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
    . cover 20 (any isTypeError (checkExp e)) "type error"
    . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
    . cover 5 (any isVariableError (checkExp e)) "variable error"
    . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
    . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
    $ ()

-- parsePrinted :: Exp -> Bool
-- parsePrinted e1 =
--   case parseAPL "" (printExp e1) of
--     Right e2 -> e1 == e2
--     Left _ -> False

parsePrinted :: Exp -> Bool
parsePrinted e1 =
  trace ("Original expression: " ++ show e1) $
  case parseAPL "" (printExp e1) of
    Right e2 -> trace ("Parsed   expression: " ++ show e2) $ e1 == e2
    Left err -> trace ("Parse   error: " ++ err) False

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage),
    ("onlyCheckedErrors", property onlyCheckedErrors),
    ("parsePrinted", property parsePrinted)
  ]
