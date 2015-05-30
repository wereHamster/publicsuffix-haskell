
module Data.PublicSuffix.Types where

import Data.Text


data Rule = Rule
    { isException :: !Bool
    , ruleLabels  :: ![Text]
    } deriving (Show)
