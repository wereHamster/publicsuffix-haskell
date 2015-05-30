{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.PublicSuffix.TH where

import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Traversable           as Traversable (forM)
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Quote  as Quasi
import qualified Language.Haskell.TH.Syntax as TH
import           System.FilePath            (dropFileName)

import           Data.PublicSuffix.Types


readRulesFile :: FilePath -> IO [Rule]
readRulesFile inputFile = do
    body <- BS.readFile inputFile
    return $ parseRules body


parseRules :: BS.ByteString -> [Rule]
parseRules body =
    map parseRule $
    filter ruleLine $
    map T.stripStart $ -- Each line is only read up to the first whitespace.
    T.lines $ -- The Public Suffix List consists of a series of lines, separated by \n.
    T.decodeUtf8 body

  where
    ruleLine line = not $ T.isPrefixOf "//" line || T.null line

    parseRule :: T.Text -> Rule
    parseRule line = case T.uncons line of
        Nothing -> error "parseRule: unexpected empty line"
        Just ('!', rest) -> Rule { isException = True,  ruleLabels = T.split (=='.') rest }
        Just (_  , rest) -> Rule { isException = False, ruleLabels = T.split (=='.') line }


moduleDirectory :: Q Exp
moduleDirectory =
    TH.LitE . TH.StringL . dropFileName . TH.loc_filename <$> TH.qLocation


mkRules :: String -> FilePath -> Q [Dec]
mkRules funName filePath = do
    rules <- runIO $ readRulesFile filePath
    rulesE <- mapM genRule rules

    return [ FunD (mkName funName) [Clause [] (NormalB $ ListE $ rulesE) []] ]

  where
    genRule :: Rule -> ExpQ
    genRule rule = do
        ruleE     <- [| Rule |]
        trueE     <- [| True |]
        falseE    <- [| False |]
        textPackE <- [| T.pack |]

        return $ foldl1 AppE
            [ ruleE
            , if isException rule then trueE else falseE
            , ListE $ reverse $ map (\x -> AppE textPackE $ LitE $ StringL x) (map T.unpack $ ruleLabels rule)
            ]
