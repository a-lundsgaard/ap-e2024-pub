fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

data Q = Sure | Nope
  deriving (Show)

val :: Q -> Int
val Sure = 1
val Nope = 0

data List a = Nil | Cons a (List a)
    deriving (Show)

listLength :: List a -> Int
listLength Nil = 0
listLength (Cons _ xs) = 1 + listLength xs

testList :: List Int
testList = Cons 1 (Cons 2 (Cons 3 Nil))