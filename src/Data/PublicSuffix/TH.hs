{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.PublicSuffix.TH where


import           Control.Applicative

import           Data.Char
import           Data.PublicSuffix.Types

import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.FilePath            (dropFileName)

import           Prelude



readRulesFile :: FilePath -> IO [Rule]
readRulesFile inputFile = do
    body <- readFile inputFile
    return $ parseRules body

isComment :: String -> Bool
isComment ('/':'/':_) = True
isComment _           = False

splitDot :: String -> [String]
splitDot [] = [""]
splitDot x  =
    let (y, z) = break (== '.') x in
    y : (if z == "" then [] else splitDot $ drop 1 z)

parseRules :: String -> [Rule]
parseRules body =
    map parseRule $
    filter ruleLine $
    map (takeWhile (not . isSpace)) $ -- Each line is only read up to the first whitespace.
    lines body -- The Public Suffix List consists of a series of lines, separated by \n.

  where
    ruleLine line = not $ isComment line || null line

    parseRule :: String -> Rule
    parseRule line = case line of
        []       -> error "parseRule: unexpected empty line"
        '!':rest -> Rule { isException = True,  ruleLabels = splitDot rest }
        _        -> Rule { isException = False, ruleLabels = splitDot line }


moduleDirectory :: Q Exp
moduleDirectory =
    TH.LitE . TH.StringL . dropFileName . TH.loc_filename <$> TH.qLocation


mkRules :: String -> FilePath -> Q [Dec]
mkRules funName filePath = do
    rules <- runIO $ readRulesFile filePath
    rulesE <- mapM genRule rules

    return
        [ SigD (mkName "rules") (AppT ListT (ConT ''Rule))
        , FunD (mkName funName) [Clause [] (NormalB $ ListE $ rulesE) []]
        ]

  where
    genRule :: Rule -> ExpQ
    genRule rule = do
        ruleE     <- [| Rule |]
        trueE     <- [| True |]
        falseE    <- [| False |]

        return $ foldl1 AppE
            [ ruleE
            , if isException rule then trueE else falseE
            , ListE $ reverse $ map (\x -> LitE $ StringL x) (ruleLabels rule)
            ]
