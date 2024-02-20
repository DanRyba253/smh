{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Actions where

import           Common               (Action, Focus (FList, FText),
                                       Focuser (FTrav), Mapping, Parser,
                                       foldMappings, lexeme, symbol,
                                       toTextUnsafe)
import           Control.Lens         ((%~), (&), (.~), (^..))
import           Control.Lens.Extras  (biplate)
import           Data.Char            (isAlphaNum)
import           Data.Functor         (($>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Parsers              (parseMappings, stringLiteral)
import           Text.Megaparsec      (MonadParsec (label, notFollowedBy),
                                       choice, satisfy)
import           Text.Megaparsec.Char (string)

parseAction :: Parser Action
parseAction = label "valid action" $ choice
    [ symbol "get-tree" $> getTree
    , symbol "get" $> actionGet
    , parseActionOver
    , parseActionSet
    ]

actionGet :: Action
actionGet input (FTrav trav) = do
    let focus = FList $ FText input ^.. trav
    printFocus focus
  where
    printFocus (FText str) = TIO.putStrLn str
    printFocus (FList lst) = mapM_ printFocus lst

actionOver :: Mapping -> Action
actionOver mapping input (FTrav trav) = do
    let output = toTextUnsafe $ FText input & trav %~ mapping
    TIO.putStr output

parseActionOver :: Parser Action
parseActionOver = do
    lexeme $ string "over" >> notFollowedBy (satisfy isAlphaNum)
    mappings <- parseMappings
    let mapping = foldMappings mappings
    return $ actionOver mapping

actionSet :: Text -> Action
actionSet str input (FTrav trav) = do
    let output = toTextUnsafe $ FText input & trav . biplate .~ str
    TIO.putStrLn output

parseActionSet :: Parser Action
parseActionSet = do
    symbol "set"
    actionSet <$> stringLiteral

getTree :: Action
getTree input (FTrav trav) = do
    putStr $ show $ FText input ^.. trav


