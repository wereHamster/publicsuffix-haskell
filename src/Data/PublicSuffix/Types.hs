module Data.PublicSuffix.Types where

data Rule = Rule
    { isException :: !Bool
      -- ^ Whether this rule is an exception or not.

    , ruleLabels  :: ![String]
      -- ^ The domain labels in reversed order, ie:
      --
      --   > "test.example.com" => ["com", "example", "test"]

    } deriving (Show)
