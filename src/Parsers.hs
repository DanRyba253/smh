{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsers where
import           Common               (Focuser (..), Mapping, Parser,
                                       Range (..), composeFocusers, focusTo,
                                       foldFocusers, foldMappings, integer,
                                       lexeme, mappingTo, rational,
                                       showRational, symbol)
import           Data.Char            (isAlphaNum)
import           Data.Functor         (($>))
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Focusers             (escaping, focusAtIdx, focusAtKey,
                                       focusAverage, focusCollect, focusCols,
                                       focusCompEq, focusCompOrd, focusConst,
                                       focusContains, focusEach, focusEl,
                                       focusEndsWith, focusFilter, focusId,
                                       focusIf, focusIndex, focusIsAlpha,
                                       focusIsAlphaNum, focusIsDigit,
                                       focusIsLower, focusIsNumber,
                                       focusIsSpace, focusIsUpper, focusKV,
                                       focusKey, focusLength, focusLines,
                                       focusLogic2, focusLogicMany, focusMaxBy,
                                       focusMaxLexBy, focusMinBy, focusMinLexBy,
                                       focusNot, focusProduct, focusRegex,
                                       focusSlice, focusSortedBy,
                                       focusSortedLexBy, focusSpace,
                                       focusStartsWith, focusSum, focusVal,
                                       focusWords)
import           Mappings             (mappingAbs, mappingAdd, mappingAppend,
                                       mappingDiv, mappingId, mappingLength,
                                       mappingLower, mappingMap, mappingMult,
                                       mappingPow, mappingPrepend,
                                       mappingReverse, mappingSign,
                                       mappingSlice, mappingSortBy,
                                       mappingSortLexBy, mappingSub,
                                       mappingUpper)
import           Text.Megaparsec      (MonadParsec (try), anySingle,
                                       anySingleBut, between, choice, empty,
                                       label, many, noneOf, notFollowedBy,
                                       optional, satisfy, sepBy, sepBy1,
                                       takeWhile1P, (<|>))
import           Text.Megaparsec.Char (char, string)

-- Focuser parsers

parseFocuser :: Parser Focuser
parseFocuser = label "valid focuser" $ choice
    [ symbol "id" $> focusId
    , symbol "each" $> focusEach
    , symbol "words" $> focusWords
    , symbol "lines" $> focusLines
    , symbol "ws" $> focusSpace
    , symbol "cols" $> focusCols
    , parseFocusSlice
    , parseFocusSortedLexBy
    , symbol "sortedLex" $> focusSortedLexBy focusId
    , parseFocusMinLexBy
    , parseFocusMaxLexBy
    , symbol "minLex" $> focusMinLexBy focusId
    , symbol "maxLex" $> focusMaxLexBy focusId
    , parseFocusSortedBy
    , parseFocusIndex
    , symbol "sorted" $> focusSortedBy focusId
    , parseFocusTo
    , symbol "len" $> focusLength
    , parseFocusMinBy
    , parseFocusMaxBy
    , symbol "min" $> focusMinBy focusId
    , symbol "max" $> focusMaxBy focusId
    , between (symbol "(") (symbol ")") $ foldFocusers <$> parseFocusers
    , symbol "sum" $> focusSum
    , symbol "product" $> focusProduct
    , parseFocusAverage
    , parseFocusAdd
    , parseFocusSub
    , parseFocusMult
    , parseFocusDiv
    , parseFocusPow
    , symbol "abs" $> focusTo mappingAbs
    , symbol "sign" $> focusTo mappingSign
    , parseFocusIf
    , symbol "isUpper" $> focusIsUpper
    , symbol "isLower" $> focusIsLower
    , symbol "isDigit" $> focusIsDigit
    , symbol "isAlphaNum" $> focusIsAlphaNum
    , symbol "isAlpha" $> focusIsAlpha
    , symbol "isSpace" $> focusIsSpace
    , symbol "isNumber" $> focusIsNumber
    , parseFocusRegex
    , parseFocusFilter
    , parseFocusContains
    , parseFocusStartsWith
    , parseFocusEndsWith
    , symbol "el" $> focusEl
    , symbol "kv" $> focusKV
    , symbol "key" $> focusKey
    , symbol "val" $> focusVal
    , parseFocusAtKey
    , parseFocusAtIdx
    , parseFocusAll
    , parseFocusAny
    , symbol "not" $> focusNot
    , parseFocusOr
    , parseFocusAnd
    , parseFocusEq
    , parseFocusNeq
    , parseFocusLeq
    , parseFocusGeq
    , try parseFocusLt <|> parseFocusCollect
    , parseFocusGt
    , parseFocusLit
    ]

parseFocusers :: Parser [Focuser]
parseFocusers = label "valid focuser stack" $ parseFocuser `sepBy1` symbol "."

parseFocusCollect :: Parser Focuser
parseFocusCollect = do
    symbol "%"
    focusCollect <$> parseFocuser

parseFocusSlice :: Parser Focuser
parseFocusSlice = do
    ranges <- between (symbol "{") (symbol "}") (range `sepBy` symbol ",")
    return $ focusSlice ranges

range :: Parser Range
range = try rangeRange <|> rangeSingle

rangeSingle :: Parser Range
rangeSingle = RangeSingle <$> integer

rangeRange :: Parser Range
rangeRange = label "range" $ do
    mstart <- optional integer
    symbol ":"
    mend <- optional integer
    return $ RangeRange mstart mend

parseFocusSortedBy :: Parser Focuser
parseFocusSortedBy = do
    lexeme $ string "sortedBy" >> notFollowedBy (satisfy isAlphaNum)
    focusSortedBy <$> parseFocuser

parseFocusIndex :: Parser Focuser
parseFocusIndex = do
    symbol "["
    n <- integer
    symbol "]"
    return $ focusIndex n

parseFocusTo :: Parser Focuser
parseFocusTo = do
    lexeme $ string "to" >> notFollowedBy (satisfy isAlphaNum)
    mapping <- foldMappings <$> parseMappings
    return $ focusTo mapping

parseFocusMinBy :: Parser Focuser
parseFocusMinBy = do
    lexeme $ string "minBy" >> notFollowedBy (satisfy isAlphaNum)
    focusMinBy <$> parseFocuser

parseFocusMaxBy :: Parser Focuser
parseFocusMaxBy = do
    lexeme $ string "maxBy" >> notFollowedBy (satisfy isAlphaNum)
    focusMaxBy <$> parseFocuser

parseFocusSortedLexBy :: Parser Focuser
parseFocusSortedLexBy = do
    lexeme $ string "sortedLexBy" >> notFollowedBy (satisfy isAlphaNum)
    focusSortedLexBy <$> parseFocuser

parseFocusMinLexBy :: Parser Focuser
parseFocusMinLexBy = do
    lexeme $ string "minLexBy" >> notFollowedBy (satisfy isAlphaNum)
    focusMinLexBy <$> parseFocuser

parseFocusMaxLexBy :: Parser Focuser
parseFocusMaxLexBy = do
    lexeme $ string "maxLexBy" >> notFollowedBy (satisfy isAlphaNum)
    focusMaxLexBy <$> parseFocuser

parseFocusAdd :: Parser Focuser
parseFocusAdd = do
    symbol "add "
    focusTo . mappingAdd <$> rational

parseFocusSub :: Parser Focuser
parseFocusSub = do
    symbol "sub "
    focusTo . mappingSub <$> rational

parseFocusMult :: Parser Focuser
parseFocusMult = do
    symbol "mult "
    focusTo . mappingMult <$> rational

parseFocusDiv :: Parser Focuser
parseFocusDiv = do
    symbol "div "
    focusTo . mappingDiv <$> rational

parseFocusPow :: Parser Focuser
parseFocusPow = do
    symbol "pow "
    focusTo . mappingPow <$> integer

parseFocusIf :: Parser Focuser
parseFocusIf = do
    lexeme $ string "if" >> notFollowedBy (satisfy isAlphaNum)
    focusIf <$> parseFocuser

parseFocusRegex :: Parser Focuser
parseFocusRegex = do
    symbol "regex"
    focusRegex <$> stringLiteral

parseFocusFilter :: Parser Focuser
parseFocusFilter = do
    lexeme $ string "filter" >> notFollowedBy (satisfy isAlphaNum)
    focusFilter <$> parseFocuser

parseFocusContains :: Parser Focuser
parseFocusContains = do
    symbol "contains"
    focusContains <$> stringLiteral

parseFocusStartsWith :: Parser Focuser
parseFocusStartsWith = do
    symbol "startsWith"
    focusStartsWith <$> stringLiteral

parseFocusEndsWith :: Parser Focuser
parseFocusEndsWith = do
    symbol "endsWith"
    focusEndsWith <$> stringLiteral

parseFocusAverage :: Parser Focuser
parseFocusAverage = do
    symbol "average"
    def <- fromMaybe 0 <$> optional rational
    return $ focusAverage def

parseFocusAtKey :: Parser Focuser
parseFocusAtKey = do
    symbol "atKey"
    focusAtKey <$> stringLiteral

parseFocusAtIdx :: Parser Focuser
parseFocusAtIdx = do
    symbol "atIdx "
    focusAtIdx <$> integer

parseFocusAll :: Parser Focuser
parseFocusAll = do
    symbol "all "
    focusLogicMany and <$> parseFocuser

parseFocusAny :: Parser Focuser
parseFocusAny = do
    symbol "any "
    focusLogicMany or <$> parseFocuser

parseFocusAnd :: Parser Focuser
parseFocusAnd = do
    symbol "&&"
    p1 <- parseFocuser
    p2 <- parseFocuser
    return $ focusLogic2 (&&) p1 p2

parseFocusOr :: Parser Focuser
parseFocusOr = do
    symbol "||"
    p1 <- parseFocuser
    p2 <- parseFocuser
    return $ focusLogic2 (||) p1 p2

parseFocusEq :: Parser Focuser
parseFocusEq = do
    symbol "="
    p1 <- parseFocuser
    mp2 <- optional parseFocuser
    return $ case mp2 of
        Just p2 -> focusCompEq (==) p1 p2
        Nothing -> focusCompEq (==) focusId p1

parseFocusNeq :: Parser Focuser
parseFocusNeq = do
    symbol "!="
    p1 <- parseFocuser
    mp2 <- optional parseFocuser
    return $ case mp2 of
        Just p2 -> focusCompEq (/=) p1 p2
        Nothing -> focusCompEq (/=) focusId p1

parseFocusLt :: Parser Focuser
parseFocusLt = do
    symbol "<"
    p1 <- parseFocuser
    mp2 <- optional parseFocuser
    return $ case mp2 of
        Just p2 -> focusCompOrd (<) p1 p2
        Nothing -> focusCompOrd (<) focusId p1

parseFocusGt :: Parser Focuser
parseFocusGt = do
    symbol ">"
    p1 <- parseFocuser
    mp2 <- optional parseFocuser
    return $ case mp2 of
        Just p2 -> focusCompOrd (>) p1 p2
        Nothing -> focusCompOrd (>) focusId p1

parseFocusLeq :: Parser Focuser
parseFocusLeq = do
    symbol "<="
    p1 <- parseFocuser
    mp2 <- optional parseFocuser
    return $ case mp2 of
        Just p2 -> focusCompOrd (<=) p1 p2
        Nothing -> focusCompOrd (<=) focusId p1

parseFocusGeq :: Parser Focuser
parseFocusGeq = do
    symbol ">="
    p1 <- parseFocuser
    mp2 <- optional parseFocuser
    return $ case mp2 of
        Just p2 -> focusCompOrd (>=) p1 p2
        Nothing -> focusCompOrd (>=) focusId p1

parseFocusLit :: Parser Focuser
parseFocusLit = parseFocusString <|> parseFocusNumber
  where
    parseFocusString = focusConst <$> stringLiteral
    parseFocusNumber = focusConst . showRational <$> rational

-- mapping parsers

parseMapping :: Parser Mapping
parseMapping = label "valid mapping" $ choice
    [ symbol "reverse" $> mappingReverse
    , symbol "len" $> mappingLength
    , parseMappingMap
    , parseMappingAppend
    , parseMappingPrepend
    , symbol "upper" $> mappingUpper
    , symbol "lower" $> mappingLower
    , between (symbol "(") (symbol ")") $ foldMappings <$> parseMappings
    , parseMappingAdd
    , parseMappingSub
    , parseMappingMult
    , parseMappingDiv
    , parseMappingPow
    , symbol "abs" $> mappingAbs
    , symbol "sign" $> mappingSign
    , parseMappingSlice
    , parseMappingSortLexBy
    , symbol "sortLex" $> mappingSortLexBy focusId
    , parseMappingSortBy
    , symbol "sort" $> mappingSortBy focusId
    , symbol "id" $> mappingId
    , parseMappingTo
    ]

parseMappings :: Parser [Mapping]
parseMappings = label "valid mapping stack" $ parseMapping `sepBy1` symbol ":"

parseMappingMap :: Parser Mapping
parseMappingMap = do
    lexeme $ string "map" >> notFollowedBy (satisfy isAlphaNum)
    mappingMap <$> parseMapping

stringLiteral :: Parser Text
stringLiteral = T.pack <$> label "string literal" (do
    between (char '"') (symbol "\"") $ many $ choice
        [ char '\\' >> anySingle
        , anySingleBut '"'
        ])

parseMappingAppend :: Parser Mapping
parseMappingAppend = do
    lexeme $ string "append" >> notFollowedBy (satisfy isAlphaNum)
    mappingAppend . foldFocusers <$> parseFocusers

parseMappingPrepend :: Parser Mapping
parseMappingPrepend = do
    lexeme $ string "prepend" >> notFollowedBy (satisfy isAlphaNum)
    mappingPrepend . foldFocusers <$> parseFocusers

parseMappingAdd :: Parser Mapping
parseMappingAdd = do
    symbol "add "
    mappingAdd <$> rational

parseMappingSub :: Parser Mapping
parseMappingSub = do
    symbol "sub "
    mappingSub <$> rational

parseMappingMult :: Parser Mapping
parseMappingMult = do
    symbol "mult "
    mappingMult <$> rational

parseMappingDiv :: Parser Mapping
parseMappingDiv = do
    symbol "div "
    mappingDiv <$> rational

parseMappingPow :: Parser Mapping
parseMappingPow = do
    symbol "pow "
    mappingPow <$> integer

parseMappingSlice :: Parser Mapping
parseMappingSlice = do
    ranges <- between (symbol "{") (symbol "}") (range `sepBy` symbol ",")
    return $ mappingSlice ranges

parseMappingSortBy :: Parser Mapping
parseMappingSortBy = do
    lexeme $ string "sortBy" >> notFollowedBy (satisfy isAlphaNum)
    mappingSortBy <$> parseFocuser

parseMappingSortLexBy :: Parser Mapping
parseMappingSortLexBy = do
    lexeme $ string "sortLexBy" >> notFollowedBy (satisfy isAlphaNum)
    mappingSortLexBy <$> parseFocuser

parseMappingTo :: Parser Mapping
parseMappingTo = do
    lexeme $ string "to" >> notFollowedBy (satisfy isAlphaNum)
    focuser <- foldFocusers <$> parseFocusers
    return $ mappingTo focuser
