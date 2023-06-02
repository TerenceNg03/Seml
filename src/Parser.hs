{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Parser (Parser, parser, runParser, (<|>), repeatP, seqP, betweenP, sepByP) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (ap)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (foldl')

newtype BaseParser i a = BaseParser {runBaseParser :: i -> (i, a)}

instance Functor (BaseParser i) where
    fmap f p = BaseParser $ \s ->
        let (s', a) = runBaseParser p s
         in (s', f a)

instance Applicative (BaseParser i) where
    pure x = BaseParser (,x)
    (<*>) = ap

instance Monad (BaseParser i) where
    return = pure
    x >>= f = BaseParser $ \s ->
        let (s', a) = runBaseParser x s
         in runBaseParser (f a) s'

newtype Parser a = Parser (ExceptT String (BaseParser String) a)
    deriving (Functor, Applicative, Monad)

parser :: (String -> (String, Either String a)) -> Parser a
parser = Parser . ExceptT . BaseParser

runParser :: Parser a -> String -> (String, Either String a)
runParser (Parser p) = runBaseParser . runExceptT $ p

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = parser $ \s ->
    case runParser p1 s of
        (_, Left _) -> runParser p2 s
        a -> a

seqP :: [Parser a] -> Parser [a]
seqP = foldl' (liftA2 $ flip (:)) (return [])

someP :: Parser a -> Parser [a]
someP p = liftA2 (:) p (manyP p)

manyP :: Parser a -> Parser [a]
manyP p = terminateP <|> someP p <|> return []

terminateP :: Parser [a]
terminateP = parser $ \s ->
    case s of
        [] -> ([], Right [])
        _ -> (s, Left "")

-- | Repeat a parser
repeatP :: Parser a -> Parser [a]
repeatP = manyP

betweenP :: Parser l -> Parser r -> Parser mid -> Parser mid
betweenP l r m = l *> m <* r

-- | Combine parser with separator
sepByP :: Parser s -> Parser a -> Parser [a]
sepByP ps pa = liftA2 (:) pa (repeatP (ps *> pa))
