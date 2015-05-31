{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad

import           Data.Monoid
import           Data.List
import           Data.PublicSuffix
import           Data.PublicSuffix.Rules

import           System.Random

import           Criterion.Main



main :: IO ()
main = do
    defaultMain
        [ bench "publicSuffix" $ nfIO $ do
            idx <- getStdRandom $ randomR (0, length rules - 1)
            let rule = rules !! idx
            let domain = "foo." <> (mconcat $ intersperse "." $ reverse $ ruleLabels rule)
            void $ return $ publicSuffix domain
        ]
