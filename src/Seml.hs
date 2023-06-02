{-# LANGUAGE ImportQualifiedPost #-}

module Seml (Seml (..), Element (..), toXML) where

import Text.XML.Light (Attr, node, unqual)
import Text.XML.Light qualified as XML

data Seml = Elem String [Seml.Element] | Tag String
    deriving (Show, Eq)
data Element = Rec Seml | Text String
    deriving (Show, Eq)

extractAttr :: [Seml.Element] -> ([Attr], [Seml.Element])
extractAttr [] = ([], [])
extractAttr (x : xs) =
    let (attrs, elems) = extractAttr xs
     in case x of
            Rec (Elem (':' : name) [Text val]) -> (XML.Attr (unqual name) val : attrs, elems)
            Text s -> case elems of
                (Text s1) : rest -> (attrs, Text (s ++ s1) : rest)
                rest -> (attrs, Text s : rest)
            _ -> (attrs, x : elems)

toElem :: Element -> XML.Element
toElem (Text s) = node (unqual s) ()
toElem (Rec (Elem tag [Text text])) = node (unqual tag) ([] :: [Attr], text)
toElem (Rec s) = toXML s

toXML :: Seml -> XML.Element
toXML (Tag t) = node (unqual t) ()
toXML (Elem tag elems) =
    let (attr, elems') = extractAttr elems
        name = unqual tag
        xmlElems = map toElem elems'
     in node name (attr, xmlElems)
