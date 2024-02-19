{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsers where
import           Common               (Comparison (..), Evaluatable (..),
                                       Focuser (..), IfExpr (..), Mapping,
                                       Oper (..), Parser, Quantor (..),
                                       Range (..), composeFocusers,
                                       foldFocusers, foldMappings, integer,
                                       lexeme, scient, symbol)
import           Data.Char            (isAlphaNum)
import           Data.Functor         (($>))
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Focusers             (focusAverage, focusCollect, focusCols,
                                       focusEach, focusId, focusIf, focusIndex,
                                       focusIsAlpha, focusIsAlphaNum,
                                       focusIsDigit, focusIsLower, focusIsSpace,
                                       focusIsUpper, focusLines, focusMaxBy,
                                       focusMaxLexBy, focusMinBy, focusMinLexBy,
                                       focusProduct, focusSlice, focusSortedBy,
                                       focusSortedLexBy, focusSpace, focusSum,
                                       focusTo, focusWords, focusRegex, focusFilter, focusContains, focusStartsWith, focusEndsWith)
import           Mappings             (mappingAbs, mappingAdd, mappingAppend,
                                       mappingDiv, mappingId, mappingLength,
                                       mappingLower, mappingMap, mappingMult,
                                       mappingPow, mappingPrepend,
                                       mappingReverse, mappingSign,
                                       mappingSlice, mappingSortBy,
                                       mappingSortLexBy, mappingSub,
                                       mappingUpper)
import           Text.Megaparsec      (MonadParsec (try), anySingle, between,
                                       choice, empty, label, many, noneOf,
                                       notFollowedBy, optional, satisfy, sepBy,
                                       sepBy1, (<|>), takeWhile1P)
import           Text.Megaparsec.Char (char, string)

-- Focuser parsers

parseFocuser :: Parser Focuser
parseFocuser = label "valid focuser" $ choice
    [ symbol "id" $> focusId
    , symbol "each" $> focusEach
    , parseFocusCollect
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
    , symbol "len" $> focusTo mappingLength
    , parseFocusMinBy
    , parseFocusMaxBy
    , symbol "min" $> focusMinBy focusId
    , symbol "max" $> focusMaxBy focusId
    , between (symbol "(") (symbol ")") $ foldFocusers <$> parseFocusers
    , symbol "sum" $> focusSum
    , symbol "product" $> focusProduct
    , symbol "average" $> focusAverage
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
    , parseFocusRegex
    , parseFocusFilter
    , parseFocusContains
    , parseFocusStartsWith
    , parseFocusEndsWith
    ]

parseFocusers :: Parser [Focuser]
parseFocusers = label "valid focuser stack" $ parseFocuser `sepBy1` symbol "."

parseFocusCollect :: Parser Focuser
parseFocusCollect = do
    symbol "<"
    focusers <- parseFocusers
    symbol ">"
    let focuser = foldFocusers focusers
    return $ focusCollect focuser

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
    mstart <- lexeme $ optional integer
    symbol ":"
    mend <- lexeme $ optional integer
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
    focusTo . mappingAdd <$> scient

parseFocusSub :: Parser Focuser
parseFocusSub = do
    symbol "sub "
    focusTo . mappingSub <$> scient

parseFocusMult :: Parser Focuser
parseFocusMult = do
    symbol "mult "
    focusTo . mappingMult <$> scient

parseFocusDiv :: Parser Focuser
parseFocusDiv = do
    symbol "div "
    focusTo . mappingDiv <$> scient

parseFocusPow :: Parser Focuser
parseFocusPow = do
    symbol "pow "
    focusTo . mappingPow <$> integer

parseFocusIf :: Parser Focuser
parseFocusIf = do
    lexeme $ string "if" >> notFollowedBy (satisfy isAlphaNum)
    ifExpr <- try parseIfExpr <|> parseIfExprShort
    return $ focusIf ifExpr

parseIfExpr :: Parser IfExpr
parseIfExpr = label "one or more blocks separated by '||'" $ do
    andBlocks <- parseAndBlock `sepBy1` symbol "||"
    case andBlocks of
        []      -> empty
        [block] -> return block
        _       -> return $ IfOr andBlocks

parseAndBlock :: Parser IfExpr
parseAndBlock = label "one or more blocks separated by '&&'" $ do
    atoms <- parseAtom `sepBy1` symbol "&&"
    case atoms of
        []     -> empty
        [atom] -> return atom
        _      -> return $ IfAnd atoms

parseAtom :: Parser IfExpr
parseAtom = between (symbol "(") (symbol ")") parseIfExpr <|> parseComp

parseComp :: Parser IfExpr
parseComp = do
    q1 <- fromMaybe QAll <$> optional parseQuantor
    lhs <- fromMaybe (EFocuser focusId) <$> optional parseEvaluatableLong
    comp <- parseCompOp
    q2 <- fromMaybe QAll <$> optional parseQuantor
    rhs <- parseEvaluatableLong
    return $ IfSingle $ Comparison (q1, lhs) comp (q2, rhs)

parseQuantor :: Parser Quantor
parseQuantor = symbol "all " $> QAll <|> symbol "any " $> QAny

parseCompOp :: Parser Oper
parseCompOp = choice
    [ symbol "=" $> OpEq
    , symbol "!=" $> OpNe
    , symbol "<=" $> OpLe
    , symbol "<"  $> OpLt
    , symbol ">=" $> OpGe
    , symbol ">"  $> OpGt
    ]

parseIfExprShort :: Parser IfExpr
parseIfExprShort = do
    q <- fromMaybe QAll <$> optional parseQuantor
    e <- parseEvaluatable
    return $ IfSingle $ Comparison (q, e) OpEq (QAny, EText "1")

parseFocusRegex :: Parser Focuser
parseFocusRegex = do
    symbol "regex"
    focusRegex <$> stringLiteral

parseFocusFilter :: Parser Focuser
parseFocusFilter = do
    lexeme $ string "filter" >> notFollowedBy (satisfy isAlphaNum)
    focusFilter <$> parseIfExpr

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
    ]

parseMappings :: Parser [Mapping]
parseMappings = label "valid mapping stack" $ parseMapping `sepBy1` symbol ">"

parseMappingMap :: Parser Mapping
parseMappingMap = do
    lexeme $ string "map" >> notFollowedBy (satisfy isAlphaNum)
    mappingMap <$> parseMapping

parseEvaluatable :: Parser Evaluatable
parseEvaluatable =
    EText <$> stringLiteral <|>
    ENumber <$> scient <|>
    EFocuser <$> parseFocuser

parseEvaluatableLong :: Parser Evaluatable
parseEvaluatableLong =
    EText <$> stringLiteral <|>
    ENumber <$> scient <|>
    EFocuser . foldFocusers <$> parseFocusers

stringLiteral :: Parser Text
stringLiteral = label "string literal" $ lexeme $ do
    char '"'
    inner <- T.concat <$> many (choice
        [ takeWhile1P Nothing (\c -> c /= '/' && c /= '"')
        , try (string "\\\"" $> "\"")
        , string "\\"
        ])
    char '"'
    return inner

parseMappingAppend :: Parser Mapping
parseMappingAppend = do
    lexeme $ string "append" >> notFollowedBy (satisfy isAlphaNum)
    mappingAppend <$> parseEvaluatableLong

parseMappingPrepend :: Parser Mapping
parseMappingPrepend = do
    lexeme $ string "prepend" >> notFollowedBy (satisfy isAlphaNum)
    mappingPrepend <$> parseEvaluatableLong

parseMappingAdd :: Parser Mapping
parseMappingAdd = do
    symbol "add "
    mappingAdd <$> scient

parseMappingSub :: Parser Mapping
parseMappingSub = do
    symbol "sub "
    mappingSub <$> scient

parseMappingMult :: Parser Mapping
parseMappingMult = do
    symbol "mult "
    mappingMult <$> scient

parseMappingDiv :: Parser Mapping
parseMappingDiv = do
    symbol "div "
    mappingDiv <$> scient

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
