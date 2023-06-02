{-# LANGUAGE TupleSections #-}

module Parser (Parser (Parser), runParser, (<|>), repeatP, betweenP, sepByP) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (ap)

-- | A very simple parser combinator
newtype Parser a = Parser {runParser :: String -> (String, Either String a)}

instance Functor Parser where
    fmap f p = Parser $ \s ->
        let (s', a) = runParser p s
         in (s', f <$> a)

instance Applicative Parser where
    pure x = Parser (,Right x)
    (<*>) = ap

instance Monad Parser where
    return = pure
    x >>= f = Parser $ \s ->
        let (s', a) = runParser x s
         in case a of
                Left err -> (s, Left err)
                Right val -> runParser (f val) s'

-- | Combine parser with an alternative one
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s ->
    case runParser p1 s of
        (_, Left _) -> runParser p2 s
        a -> a

someP :: Parser a -> Parser [a]
someP p = liftA2 (:) p (manyP p)

manyP :: Parser a -> Parser [a]
manyP p = terminateP <|> someP p <|> return []

terminateP :: Parser [a]
terminateP = Parser $ \s ->
    case s of
        [] -> ([], Right [])
        _ -> (s, Left "")

-- | Repeat a parser
repeatP :: Parser a -> Parser [a]
repeatP = manyP

-- | Parse something between other things
betweenP :: Parser l -> Parser r -> Parser mid -> Parser mid
betweenP l r m = l *> m <* r

-- | Repeat parser with separator
sepByP :: Parser s -> Parser a -> Parser [a]
sepByP ps pa = liftA2 (:) pa (repeatP (ps *> pa))
