module APL.Eval_Tests where

import Test.Tasty
import Test.Tasty.HUnit

import APL.Eval
import APL.AST

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Evaluation Tests"
    [ testCase "Evaluating Div (CstInt 6) (CstInt 3)" $
        eval envEmpty (Div (CstInt 6) (CstInt 3)) @?= Right (ValInt 2)
    , testCase "Evaluating Div (CstInt 6) (CstInt 0)" $
        eval envEmpty (Div (CstInt 6) (CstInt 0)) @?= Left "Division by zero"
    , testCase "Evaluating Pow (CstInt 2) (CstInt 3)" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3)) @?= Right (ValInt 8)
    , testCase "Evaluating Pow (CstInt 2) (CstInt -1)" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1))) @?= Left "Negative exponent"
    , testCase "Evaluating If with true condition" $
        eval envEmpty (If (CstBool True) (CstInt 1) (CstInt 0)) @?= Right (ValInt 1)
    , testCase "Evaluating If with false condition" $
        eval envEmpty (If (CstBool False) (CstInt 1) (CstInt 0)) @?= Right (ValInt 0)
    , testCase "Evaluating If with non-boolean condition" $
        eval envEmpty (If (CstInt 1) (CstInt 1) (CstInt 0)) @?= Left "Condition in if expression must be a boolean"
    , testCase "Evaluating If with Eql condition true" $
        eval envEmpty (If (Eql (CstInt 1) (CstInt 1)) (CstInt 1) (CstInt 0)) @?= Right (ValInt 1)
    , testCase "Evaluating If with Eql condition false" $
        eval envEmpty (If (Eql (CstInt 1) (CstInt 2)) (CstInt 1) (CstInt 0)) @?= Right (ValInt 0)
    , testCase "Evaluating If with Eql condition type error" $
        eval envEmpty (If (Eql (CstInt 1) (CstBool True)) (CstInt 1) (CstInt 0)) @?= Left "Invalid operands to equality"
    , testCase "Evaluating Var with existing variable" $
        eval [("x", ValInt 42)] (Var "x") @?= Right (ValInt 42)
    , testCase "Evaluating Var with non-existing variable" $
        eval envEmpty (Var "x") @?= Left "Variable x not found"
    , testCase "Evaluating Let expression" $
        eval envEmpty (Let "x" (CstInt 5) (Add (Var "x") (CstInt 3))) @?= Right (ValInt 8)
    , testCase "Evaluating nested Let expressions" $
        eval envEmpty (Let "x" (CstInt 5) (Let "y" (CstInt 3) (Add (Var "x") (Var "y")))) @?= Right (ValInt 8)
    ]