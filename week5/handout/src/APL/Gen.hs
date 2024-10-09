-- import       , tasty-quickcheck
module APL.Gen where

import Test.QuickCheck (Gen)
import APL.AST
import Test.QuickCheck

genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

genExp :: Gen Exp
genExp = oneof [Var <$> genVar, Lambda <$> genVar <*> genExp]