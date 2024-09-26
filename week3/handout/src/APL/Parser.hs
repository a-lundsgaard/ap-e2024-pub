module APL.Parser (parseAPL, lInteger) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String

lVName :: Parser VName
lVName = lexeme $ try $ do -- Try to parse a variable name
  c <- satisfy isAlpha -- Parse an alphabetic character
  cs <- many $ satisfy isAlphaNum -- Parse zero or more alphanumeric characters
  let v = c : cs -- Combine the first character and the rest of the characters into a single string
  if v `elem` keywords -- Check if the variable name is a keyword
    then fail "Unexpected keyword" -- If it is, fail the parser
    else pure v

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]


lKeyword :: String -> Parser ()
lKeyword s = lexeme $ 
  void $ -- The void function discards the result of a parser, returning () instead.
  try $  -- The try function allows the parser to backtrack if it fails after consuming input.
  chunk s -- creates a parser that matches the exact string s.
    <* -- <* runs the parser on its left (chunk s) and then the parser on its right: (notFollowedBy) - returns the result of the left parser.
    notFollowedBy (satisfy isAlphaNum)


lBool :: Parser Bool
lBool =
  {- 
  The try function allows the parser to backtrack if it fails after consuming input. This is useful when you have multiple parsers that can consume the same prefix of input but diverge later.
  In this case, if parsing true fails after consuming some input, try ensures that the input is not consumed, allowing the parser to attempt parsing false.
   -}
  try $ lexeme $
    choice
      {- 
      The choice function tries each parser in the list in order until one succeeds.
      It takes a list of parsers and returns a parser that tries each parser in the list in sequence, returning the result of the first successful parser.
      -}
      [ const True <$> lKeyword "true", -- const :: a -> b -> a
        const False <$> lKeyword "false"
      ]

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

lInteger :: Parser Integer
lInteger = lexeme $ do
  n <- some (satisfy isDigit)  -- Parse one or more digits and bind the result to n
  notFollowedBy (satisfy isAlpha)  -- Ensure the parsed digits are not followed by an alphabetic character
  pure (read n)  -- Convert the parsed string of digits to an Integer and return it
  
lexeme :: Parser a -> Parser a
lexeme p = p <* space -- Parse p and consume trailing whitespace


lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

{- 
Atom ::= var
       | int
       | bool
       | "(" Exp ")"

Exp0' ::=            (* empty *)
        | "+" Atom Exp0'
        | "-" Atom Exp0'
        | "*" Atom Exp0'
        | "/" Atom Exp0'

Exp0 ::= Atom Exp0'

Exp  ::= Exp0
 -}
pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      (lString "(" *> pExp) <* lString ")"
    ]

-- pExp0 :: Parser Exp
-- pExp0 = pAtom >>= chain
--   where
--     chain x =
--       choice
--         [ do
--             lString "+"
--             y <- pAtom
--             chain $ Add x y,
--           do
--             lString "-"
--             y <- pAtom
--             chain $ Sub x y,
--           do
--             lString "*"
--             y <- pAtom
--             chain $ Mul x y,
--           do
--             lString "/"
--             y <- pAtom
--             chain $ Div x y,
--           pure x
--         ]


pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pAtom
            chain $ Mul x y,
          do
            lString "/"
            y <- pAtom
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0


-- Implement if-then-else
pLExp :: Parser Exp
pLExp =
  choice
    [ If <$> (lKeyword "if" *> pExp0) -- Parse the keyword if followed by an expression
        <*> (lKeyword "then" *> pExp0)
        <*> (lKeyword "else" *> pExp0),
      pAtom
    ]

