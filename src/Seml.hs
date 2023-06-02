module Seml (Seml (..), Element (..)) where

data Seml = Elem String [Element] | Tag String
    deriving (Show, Eq)
data Element = Rec Seml | Text String
    deriving (Show, Eq)
