{-# LANGUAGE OverloadedStrings #-}

module Data.PublicSuffix
    ( Rule(..)
    , publicSuffix
    , registeredDomain
    ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Function
import           Data.List

import           Data.PublicSuffix.Types
import           Data.PublicSuffix.Rules



-- A domain is said to match a rule if and only if all of the following
-- conditions are met:
--
--  - When the domain and rule are split into corresponding labels, that the
--    domain contains as many or more labels than the rule.
--
--  - Beginning with the right-most labels of both the domain and the rule,
--    and continuing for all labels in the rule, one finds that for every pair,
--    either they are identical, or that the label from the rule is "*".

matchRule :: Text -> Rule -> Bool
matchRule domain rule =
    domainLabelsLength >= ruleLabelsLength &&
    all labelMatches (zip (ruleLabels rule) (reverse domainLabels))

  where
    ruleLabelsLength   = length $ ruleLabels rule

    domainLabels       = T.split (=='.') domain
    domainLabelsLength = length domainLabels


-- | True if the label from the rule matches a label from the domain.
labelMatches :: (Text, Text) -> Bool
labelMatches ("*"      , _          ) = True
labelMatches (ruleLabel, domainLabel) = ruleLabel == domainLabel



-- | Return the public suffix of the given domain name (a dot-delimited unicode
-- string). The public suffix is the part of a domain which should be protected.
--
-- Notes:
--
--  - The domain MUST NOT start with a dot. Normalize the domain before passing
--    it to functions in this module.
--  - The domain MUST NOT be in punycode encoding. The matching of domain labels
--    is done on the original encoding, as specified in the upstream
--    publicsuffix list.
--
--
-- In particular that means applications SHOULD reject:
--
--  - HTTP cookies which try to set domain to a public suffix.
--  - X509 wildcard certificates which try to match all subdomains of a public
--    suffix.

publicSuffix :: Text -> Text
publicSuffix domain =
    -- Algorithm (see https://publicsuffix.org/list/)
    --
    --   Match domain against all rules and take note of the matching ones.
    --   If no rules match, the prevailing rule is "*".
    --   If more than one rule matches, the prevailing rule is the one which is an exception rule.
    --   If there is no matching exception rule, the prevailing rule is the one with the most labels.
    --   If the prevailing rule is a exception rule, modify it by removing the leftmost label.
    --   The public suffix is the set of labels from the domain which match the labels of the prevailing rule, using the matching algorithm above.
    --   The registered or registrable domain is the public suffix plus one additional label.

    mconcat $ intersperse "." $ reverse $ take numMatchingLabels $ reverse domainLabels

  where
    rule              = prevailingRule domain
    domainLabels      = T.split (=='.') domain
    numMatchingLabels = length $ takeWhile labelMatches $ zip (ruleLabels rule) (reverse domainLabels)


prevailingRule :: Text -> Rule
prevailingRule domain = case filter (matchRule domain) rules of
    []  -> Rule False ["*"]
    [x] -> x
    xs  -> case filter isException xs of
        []   -> head $ reverse $ sortBy (compare `on` (length . ruleLabels)) xs
        ex:_ -> Rule (isException ex) (init $ ruleLabels ex)



-- | Return the domain that was registered or is registrable by a user. These
-- domains are fully controlled by users, and applications SHOULD accept
-- cookies and wildcard certificates for those.

registeredDomain :: Text -> Maybe Text
registeredDomain domain = if domain == suffix
    then Nothing
    else Just $ mconcat $ intersperse "." $ reverse $ take (suffixLabelsLength + 1) $ reverse domainLabels

  where
    suffix             = publicSuffix domain
    suffixLabelsLength = length $ T.split (=='.') suffix
    domainLabels       = T.split (=='.') domain
