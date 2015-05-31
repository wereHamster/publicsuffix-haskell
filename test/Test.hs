{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import           Data.Foldable
import           Data.Monoid

import           Data.PublicSuffix
import           Data.PublicSuffix.Rules

import           Test.Hspec

import           Prelude


main :: IO ()
main = hspec spec

regularRules, exceptionRules :: [Rule]
regularRules   = filter (not . isException) rules
exceptionRules = filter isException rules

spec :: Spec
spec = parallel $ do

    describe "publicSuffix" $ do
        it "should return an empty string if the domain is null" $ do
           publicSuffix "" `shouldBe` ""

        it "should return the correct suffix when given an exception rule" $ do
            forM_ exceptionRules $ \rule -> do
                let domain = mconcat $ intersperse "." $ reverse $ ruleLabels rule
                let suffix = mconcat $ intersperse "." $ reverse $ init $ ruleLabels rule
                publicSuffix domain `shouldBe` suffix

        it "should return the correct suffix when given an registered domain" $ do
            forM_ regularRules $ \rule -> do
                let suffix = mconcat $ intersperse "." $ reverse $ ruleLabels rule
                publicSuffix ("x99b016dd9783a7c49a." ++ suffix) `shouldBe` suffix


    describe "registeredDomain" $ do
        it "should return Nothing if the domain is a suffix" $ do
            forM_ regularRules $ \rule -> do
                let suffix = mconcat $ intersperse "." $ reverse $ ruleLabels rule
                registeredDomain suffix `shouldBe` Nothing

        it "should return the Just the domain if it is an exception" $ do
            forM_ exceptionRules $ \rule -> do
                let domain = mconcat $ intersperse "." $ reverse $ ruleLabels rule
                registeredDomain domain `shouldBe` (Just domain)

        it "should return the domain when given an registered domain" $ do
            forM_ regularRules $ \rule -> do
                let suffix = mconcat $ intersperse "." $ reverse $ ruleLabels rule
                let domain = "x99b016dd9783a7c49a." ++ suffix
                registeredDomain domain `shouldBe` (Just domain)

        it "should return the registered domain when given a subdomain of a registered domain" $ do
            forM_ regularRules $ \rule -> do
                let suffix    = mconcat $ intersperse "." $ reverse $ ruleLabels rule
                let domain    = "x99b016dd9783a7c49a." ++ suffix
                let subDomain = "sub." ++ domain
                registeredDomain subDomain `shouldBe` (Just domain)
