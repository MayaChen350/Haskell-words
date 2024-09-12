module Enum (Difficulty(..)) where
data Difficulty = Easy
                | Normal
                | Hard
                | Hardcore
                | SigmaMale
        deriving (Show, Read, Eq)