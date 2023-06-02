module Syntax (semlP, wordsP, quotedP) where

import Data.Char (isSpace)
import Parser (Parser, betweenP, parser, repeatP, sepByP, (<|>))
import Seml (Element (Rec, Text), Seml (Elem, Tag))

charP :: (Char -> Bool) -> Parser Char
charP f = parser $ \s ->
    case s of
        (x : xs)
            | f x -> (xs, Right x)
            | otherwise -> (s, Left ("Invalid Char '" ++ x : ['\'']))
        [] -> (s, Left "Eof Error")

escapeSP :: Parser Char
escapeSP = isP '\\' *> charP (const True)

spaceP :: Parser String
spaceP = repeatP (charP isSpace)

-- | Parse word with escaped white space
wordP :: Parser String
wordP = repeatP (escapeSP <|> charP (not . \x -> isSpace x || x == ')' || x == '('))

isP :: Char -> Parser Char
isP c = charP (== c)

quotedP :: Parser String
quotedP =
    let ssP = repeatP (escapeSP <|> charP (/= '\''))
        dsP = repeatP (escapeSP <|> charP (/= '"'))
        singleP = betweenP (isP '\'') (isP '\'') ssP
        doubleP = betweenP (isP '"') (isP '"') dsP
     in singleP <|> doubleP

-- | Parse words separated by white space
wordsP :: Parser [String]
wordsP = sepByP spaceP wordP

semlP :: Parser Seml
semlP = tagP <|> elemsP

tagP :: Parser Seml
tagP =
    let tag = isP '(' *> spaceP *> (wordP <|> quotedP) <* spaceP <* isP ')'
     in Tag <$> tag

elemP :: Parser Element
elemP = let elemWordP = Text <$> wordP
            elemStringP = Text <$> quotedP
            elemRecP = Rec <$> semlP
        in elemWordP <|> elemStringP <|> elemRecP

elemsP :: Parser Seml
elemsP = do
    tag <- isP '(' *> spaceP *> (wordP <|> quotedP) <* spaceP
    elem <- repeatP (elemP <* spaceP) <* isP ')'
    return (Elem tag elem)
