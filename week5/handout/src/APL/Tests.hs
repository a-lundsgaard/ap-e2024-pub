{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module APL.Tests where

import APL.AST (Exp (..), VName)
import APL.Eval
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements, listOf, oneof, sample, sized, quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary(shrink))

genVar :: Gen VName
genVar = do
  alpha <- elements ['a' .. 'z']
  alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
  pure (alpha : alphaNums)

-- genExp :: Gen Exp
-- genExp = oneof [Var <$> genVar, Lambda <$> genVar <*> genExp]

-- genExp :: Gen Exp
-- genExp = oneof [ Var <$> genVar
--                , Lambda <$> genVar <*> genExp
--                , Apply <$> genExp <*> genExp
--                ]

-- sized :: (Int -> Gen a) -> Gen a
-- sized f =
-- Modified genExp to take a size parameter

{-
Why (n \div` 2)`?
Preventing Exponential Growth:
    If we used n for both sub-expressions, the size could grow exponentially.
    For example, Apply (genExp n) (genExp n) would result in a size of 2n, 4n, 8n, etc.

 -}

-- Arbitrary instance for Exp
instance Arbitrary Exp where
  arbitrary = sized genExp
  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Var x) =
    [Var x' | x' <- shrink x, not (null x')]
  shrink (Let x e1 e2) =
    e1 : [Let x' e1 e2 | x' <- shrink x, not (null x')] ++ [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    e : [Lambda x' e | x' <- shrink x, not (null x')] ++ [Lambda x e' | e' <- shrink e]

genExp :: Int -> Gen Exp
genExp 0 = Var <$> genVar
genExp 1 = Var <$> genVar
genExp n = do
  let half = (n - 1) `div` 2
  oneof
    [ Var <$> genVar,
      Lambda <$> genVar <*> genExp (n - 1),
      Apply <$> genExp half <*> genExp half,
      If <$> genExp half <*> genExp half <*> genExp half,
      Let <$> genVar <*> genExp half <*> genExp half,
      Lambda <$> genVar <*> genExp (n - 1),
      Apply <$> genExp half <*> genExp half,
      If <$> genExp half <*> genExp half <*> genExp half,
      Let <$> genVar <*> genExp half <*> genExp half,
      Add <$> genExp half <*> genExp half,
      Sub <$> genExp half <*> genExp half,
      Mul <$> genExp half <*> genExp half,
      Div <$> genExp half <*> genExp half,
      Pow <$> genExp half <*> genExp half,
      Eql <$> genExp half <*> genExp half,
      TryCatch <$> genExp half <*> genExp half
    ]



prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc x y z = (x + y) + z == x + (y + z)

-- run quickCheck prop_integerAddAssoc to test this.

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))

test_integerAddAssoc :: IO ()
test_integerAddAssoc = do 
    quickCheck prop_integerAddAssoc
    quickCheck prop_aplAddAssoc


main :: Int -> IO ()
main i = do
  putStrLn "Sample output for genVar:"
  sample genVar
  putStrLn "\nSample output for genExp:"
  sample $ genExp i


-- cabal repl
-- import APL.AST
-- :l Tests.hs OR :l src/APL/Tests.hs
-- main