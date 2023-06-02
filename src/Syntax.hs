module Syntax (semlP) where

import Control.Applicative (Applicative (liftA2))
import Data.Char (isSpace)
import Parser (Parser (Parser), betweenP, repeatP, sepByP, (<|>))
import Seml (Element (Rec, Text), Seml (Elem, Tag))

charP :: (Char -> Bool) -> Parser Char
charP f = Parser $ \s ->
    case s of
        (x : xs)
            | f x -> (xs, Right x)
            | otherwise -> (s, Left ("Invalid Char '" ++ x : ['\'']))
        [] -> (s, Left "Eof Error")

escapeSP :: Parser Char
escapeSP = isP '\\' *> charP (const True)

-- | Parse white spaces
spaceP :: Parser String
spaceP = repeatP (charP isSpace)

-- | Parse word with escaped white space
wordP :: Parser String
wordP =
    let wcP = escapeSP <|> charP (not . \x -> isSpace x || x == ')' || x == '(')
     in liftA2 (:) wcP (repeatP wcP)

isP :: Char -> Parser Char
isP c = charP (== c)

-- | Parse quoted string
quotedP :: Parser String
quotedP =
    let ssP = repeatP (escapeSP <|> charP (/= '\''))
        dsP = repeatP (escapeSP <|> charP (/= '"'))
        singleP = betweenP (isP '\'') (isP '\'') ssP
        doubleP = betweenP (isP '"') (isP '"') dsP
     in singleP <|> doubleP

semlOneP :: Parser Seml
semlOneP =
    let tagP' = Tag <$> tagP
        elemsP' = liftA2 Elem (tagP <* spaceP) elemsP
        betweenPP = betweenP (isP '(' <* spaceP) (spaceP *> isP ')')
     in betweenPP tagP' <|> betweenPP elemsP'

-- | Seml Parser
semlP :: Parser [Seml]
semlP = repeatP $ spaceP *> semlOneP <* spaceP

tagP :: Parser String
tagP = quotedP <|> wordP

elemP :: Parser Element
elemP =
    let elemWordP = Text <$> wordP
        elemStringP = Text <$> quotedP
        elemRecP = Rec <$> semlOneP
     in elemWordP <|> elemStringP <|> elemRecP

elemsP :: Parser [Element]
elemsP = sepByP spaceP elemP
