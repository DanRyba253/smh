{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Actions              (parseAction)
import           Common               (Action, Focuser, Parser, foldFocusers,
                                       symbol)
import           Control.Monad        (when)
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Parsers              (parseFocusers)
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)
import           Text.Megaparsec      (errorBundlePretty, many, optional, parse)
import           Text.Megaparsec.Char (char)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1 && length args /= 2) $ do
        putStrLn "usage: smh <command> [input]"
        exitFailure
    (focusers, action) <- case parse parseData "input" (T.pack $ head args) of
        Left errors -> do
            putStrLn $ errorBundlePretty errors
            exitFailure
        Right x -> pure x

    let focuser = foldFocusers focusers
    input <- if length args == 1
        then TIO.getContents
        else return $ T.pack $ args !! 1
    action input focuser


parseData :: Parser ([Focuser], Action)
parseData = do
    many (char ' ')
    focusers <- fromMaybe [] <$> optional parseFocusers
    symbol "|"
    action <- parseAction
    return (focusers, action)


