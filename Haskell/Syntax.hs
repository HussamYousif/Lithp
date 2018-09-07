module Syntax where

import Text.ParserCombinators.Parsec hiding (spaces)


data Atom = AStr String
          | ABool Bool
          | ANum Int
          | ANil
          deriving (Eq, Show, Ord)

data AST = Node [AST] | Leaf Atom


keywords = ["if"]


