{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.PublicSuffix
import Criterion.Main



main :: IO ()
main = do
    defaultMain
        [ bgroup "publicSuffix" $ map (\d -> bench d $ nf publicSuffix d)
            [ "ac", "zurich", "dontexist"
            ]
        ]
