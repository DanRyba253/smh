{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Focusers where

import           Common               (Comparison (..), Evaluatable (..),
                                       Focus (..), Focuser (..), IfExpr (..),
                                       Mapping, Oper (..), Parser, Quantor (..),
                                       Range (RangeSingle), _toListUnsafe,
                                       composeFocusers, fromIndexes, getIndexes,
                                       lexeme, makeFilteredText, mapText,
                                       readMaybeRational, showRational, symbol,
                                       toListUnsafe, toTextUnsafe, unsort, ws)
import           Control.Applicative  ((<|>))
import           Control.Lens         (lens, partsOf, (^..))
import           Control.Monad        (void, when, zipWithM)
import           Data.Char            (isAlpha, isAlphaNum, isDigit, isLower,
                                       isSpace, isUpper)
import           Data.Data.Lens       (biplate)
import           Data.Function        (on)
import           Data.Functor         ((<&>))
import           Data.List            (sortBy, transpose)
import           Data.Maybe           (mapMaybe)
import           Data.Ord             (comparing)
import           Data.Ratio           (denominator)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.Megaparsec      (anySingle, anySingleBut, between, choice,
                                       empty, getOffset, many, optional,
                                       parseMaybe, satisfy, sepBy, some, try)
import           Text.Megaparsec.Char (char)
import           Text.Read            (readMaybe)
import           Text.Regex.PCRE      (AllMatches (getAllMatches), (=~))
import           Text.Regex.PCRE.Text ()

focusId :: Focuser
focusId = FTrav id

focusEach :: Focuser
focusEach = FTrav traverseFocus

traverseFocus :: Applicative f => (Focus -> f Focus) -> (Focus -> f Focus)
traverseFocus f focus = case focus of
    FText str -> FText . T.concat . map toTextUnsafe <$> traverse f (mapText (FText . T.singleton) str)
    FList lst -> FList <$> traverse f lst

focusCollect :: Focuser -> Focuser
focusCollect (FTrav innerTrav) = FTrav $ partsOf innerTrav . _toListUnsafe

focusWords :: Focuser
focusWords = FTrav wordsTrav

wordsTrav :: Applicative f => (Focus -> f Focus) -> (Focus -> f Focus)
wordsTrav _ flst@(FList _) = pure flst
wordsTrav f (FText str) =
    let (str_ws, str_words) = myWords str
        new_words = map toTextUnsafe <$> traverse (f . FText) str_words
        new_str = T.concat . interleave str_ws <$> new_words
    in  FText <$> new_str

myWords :: Text -> ([Text], [Text])
myWords "" = ([], [])
myWords str =
    let (ws, str') = T.span isSpace str
        (word, str'') = T.break isSpace str'
        (str_ws, str_words) = myWords str''
    in  (ws : str_ws, if not (T.null word) then word : str_words else str_words)

focusSpace :: Focuser
focusSpace = FTrav spaceTrav

spaceTrav :: Applicative f => (Focus -> f Focus) -> (Focus -> f Focus)
spaceTrav _ flst@(FList _) = pure flst
spaceTrav f (FText str) =
    let (str_nonspace, str_space) = mySpace str
        new_space = map toTextUnsafe <$> traverse (f . FText) str_space
        new_str = T.concat . interleave str_nonspace <$> new_space
    in  FText <$> new_str

mySpace :: Text -> ([Text], [Text])
mySpace "" = ([], [])
mySpace str =
    let (nonspace, str') = T.break isSpace str
        (space, str'') = T.span isSpace str'
        (str_nonspace, str_space) = mySpace str''
    in  (nonspace : str_nonspace, if not (T.null space) then space : str_space else str_space)

interleave :: [a] -> [a] -> [a]
interleave [] a2s                = a2s
interleave a1s []                = a1s
interleave (a1 : a1s) (a2 : a2s) = a1 : a2 : interleave a1s a2s

focusLines :: Focuser
focusLines = FTrav linesTrav

linesTrav :: Applicative f => (Focus -> f Focus) -> (Focus -> f Focus)
linesTrav _ flst@(FList _) = pure flst
linesTrav f (FText str) = FText . T.concat . map ((`T.append` "\n") . toTextUnsafe)
    <$> traverse (f . FText) (T.lines str)

transposeTravUnsafe :: Applicative f => (Focus -> f Focus) -> (Focus -> f Focus)
transposeTravUnsafe f flist = transposeFListUnsafe <$> f (transposeFListUnsafe flist)

transposeFListUnsafe :: Focus -> Focus
transposeFListUnsafe (FList lst) = FList . map FList $ transpose (toListUnsafe <$> lst)
transposeFListUnsafe _ =
    error "smh: transposeFListUnsafe called on a non-FList. Please, report this bug."

focusCols :: Focuser
focusCols = focusCollect (focusLines `composeFocusers` focusCollect focusWords)
    `composeFocusers` FTrav transposeTravUnsafe
    `composeFocusers` focusEach

focusSlice :: [Range] -> Focuser
focusSlice ranges = FTrav $ \f focus -> case focus of
    FText str -> FText <$> new_str
      where
        str_length = T.length str
        is = getIndexes ranges str_length
        filtered_str = makeFilteredText str_length is str
        new_filtered_str = toTextUnsafe <$> (f . FText $ filtered_str)
        new_str = updateText str is <$> new_filtered_str

    FList lst -> FList <$> new_lst
      where
        is = getIndexes ranges (length lst)
        filtered_lst = makeFilteredList is 0 lst
        new_filtered_list = toListUnsafe <$> (f . FList $ filtered_lst)
        new_lst = updateList lst . zip is <$> new_filtered_list
  where
    makeFilteredList [] _ _ = []
    makeFilteredList _ _ [] = []
    makeFilteredList (i : is) idx (c : str)
        | idx == i = c : makeFilteredList is (idx + 1) str
        | otherwise = makeFilteredList (i : is) (idx + 1) str

    updateList :: [a] -> [(Int, a)] -> [a]
    updateList as updates = aux (zip [0..] as) updates
      where
        aux old [] = map snd old
        aux [] _ = []
        aux ((i, a) : old) ((j, a') : updates)
            | i == j = a' : aux old updates
            | otherwise = a : aux old ((j, a') : updates)

    updateText :: Text -> [Int] -> Text -> Text
    updateText old is new = T.unfoldrN (oldLen + newLen) builder (0, 0, is)
      where
        newLen = T.length new
        oldLen = T.length old

        builder :: (Int, Int, [Int]) -> Maybe (Char, (Int, Int, [Int]))
        builder (oldI, newI, [])
            | newI < newLen = Just (T.index new newI, (oldI, newI + 1, []))
            | oldI < oldLen = Just (T.index old oldI, (oldI + 1, newI, []))
            | otherwise = Nothing
        builder (oldI, newI, i : is)
            | oldI == i = if newI < newLen
                then Just (T.index new newI, (oldI + 1, newI + 1, is))
                else builder (oldI + 1, newI + 1, is)
            | otherwise = Just (T.index old oldI, (oldI + 1, newI, i : is))

focusSortedBy :: Focuser -> Focuser
focusSortedBy (FTrav trav) = FTrav $ \f focus -> case focus of
    FText str ->
        let str_length = T.length str
            (is, sorted_str) = unzip $ sortBy (cmp `on` (FText . T.singleton . snd)) $
                zip [0..] $ T.unpack str
            new_sorted_str = toTextUnsafe <$> (f . FText) ( T.pack sorted_str)
            unsort_is = unsort is str_length
            new_str = unsortText unsort_is str_length <$> new_sorted_str
        in  FText <$> new_str
    FList lst ->
        let (is, sorted_lst) = unzip $ sortBy (cmp `on` snd) $ zip [0..] lst
            new_sorted_lst = toListUnsafe <$> (f . FList) sorted_lst
            new_lst = map snd . sortBy (comparing fst) . zip is <$> new_sorted_lst
        in  FList <$> new_lst
  where
    cmp f1 f2 =
        let f1' = f1 ^.. trav
            f2' = f2 ^.. trav
        in case (f1', f2') of
            ([FText s1], [FText s2]) -> case (readMDouble s1, readMDouble s2) of
                (Just n1, Just n2) -> compare n1 n2
                _                  -> EQ
            _ -> EQ

    unsortText :: [Int] -> Int -> Text -> Text
    unsortText is strLen str = T.unfoldrN strLen builder is
      where
        builder :: [Int] -> Maybe (Char, [Int])
        builder []       = Nothing
        builder (i : is) = Just (T.index str i, is)


    readMDouble :: Text -> Maybe Double
    readMDouble = readMaybe . T.unpack

focusIndex :: Int -> Focuser
focusIndex n_ = FTrav $ \f focus -> case focus of
    FText str -> if n < 0 || n >= T.length str then pure focus else
        (f . FText . T.singleton) (T.index str n) <&> \new_str ->
            case toTextUnsafe new_str of
                ""   -> FText str
                text -> FText $ updateTextAt str_length str n (T.head text)
      where
        str_length = T.length str
        n = if n_ < 0 then str_length + n_ else n_
    FList lst -> if n < 0 || n >= length lst then pure focus else
        let new_focus = f (lst !! n)
            in FList . updateListAt lst n <$> new_focus
      where
        n = if n_ < 0 then length lst + n_ else n_
  where
    updateListAt :: [a] -> Int -> a -> [a]
    updateListAt [] _ _         = []
    updateListAt (_ : olds) 0 a = a : olds
    updateListAt (o : olds) n a = o : updateListAt olds (n - 1) a

    updateTextAt :: Int -> Text -> Int -> Char -> Text
    updateTextAt strLen str i newC = T.unfoldrN strLen builder 0
      where
        builder :: Int -> Maybe (Char, Int)
        builder n
            | n >= strLen = Nothing
            | n == i    = Just (newC, n + 1)
            | otherwise = Just (T.index str n, n + 1)

focusMinBy :: Focuser -> Focuser
focusMinBy f = focusSortedBy f `composeFocusers` focusIndex 0

focusMaxBy :: Focuser -> Focuser
focusMaxBy f = focusSortedBy f `composeFocusers` focusIndex (-1)

focusSortedLexBy :: Focuser -> Focuser
focusSortedLexBy (FTrav trav) = FTrav $ \f focus -> case focus of
    FText str ->
        let (is, sorted_str) = unzip $ sortBy (cmp `on` (FText . T.singleton . snd)) $
                zip [0..] $ T.unpack str
            str_length = T.length str
            new_sorted_str = toTextUnsafe <$> (f . FText . T.pack) sorted_str
            unsort_is = unsort is str_length
            new_str = unsortText unsort_is str_length <$> new_sorted_str
        in  FText <$> new_str
    FList lst ->
        let (is, sorted_lst) = unzip $ sortBy (cmp `on` snd) $ zip [0..] lst
            new_sorted_lst = toListUnsafe <$> (f . FList) sorted_lst
            new_lst = map snd . sortBy (comparing fst) . zip is <$> new_sorted_lst
        in  FList <$> new_lst
  where
    cmp f1 f2 =
        let f1' = f1 ^.. trav
            f2' = f2 ^.. trav
        in case (f1', f2') of
            ([FText s1], [FText s2]) -> compare s1 s2
            _                        -> EQ

    unsortText :: [Int] -> Int -> Text -> Text
    unsortText is strLen str = T.unfoldrN strLen builder is
      where
        builder :: [Int] -> Maybe (Char, [Int])
        builder []       = Nothing
        builder (i : is) = Just (T.index str i, is)

focusMinLexBy :: Focuser -> Focuser
focusMinLexBy f = focusSortedLexBy f `composeFocusers` focusIndex 0

focusMaxLexBy :: Focuser -> Focuser
focusMaxLexBy f = focusSortedLexBy f `composeFocusers` focusIndex (-1)

focusSum :: Focuser
focusSum = FTrav $ lens getSum const

getSum :: Focus -> Focus
getSum focus = case focus of
    FList _ -> FText $ showRational $ sum $
        mapMaybe readMaybeRational $ focus ^.. biplate
    FText s -> FText $ showRational $ sum $
        mapMaybe (readMaybeRational . T.singleton) $ T.unpack s

focusProduct :: Focuser
focusProduct = FTrav $ lens getProduct const

getProduct :: Focus -> Focus
getProduct focus = case focus of
    FList _ -> FText $ showRational $ product $
        mapMaybe readMaybeRational $ focus ^.. biplate
    FText s -> FText $ showRational $ product $
        mapMaybe (readMaybeRational . T.singleton) $ T.unpack s

focusAverage :: Rational -> Focuser
focusAverage n = FTrav $ lens (getAverage n) const

getAverage :: Rational -> Focus -> Focus
getAverage n focus = case focus of
    FList _ -> FText $ showRational $ average n $
        mapMaybe readMaybeRational $ focus ^.. biplate
    FText s -> FText $ showRational $ average n $
        mapMaybe (readMaybeRational . T.singleton) $ T.unpack s

average :: Rational -> [Rational] -> Rational
average n [] = n
average _ xs = sum xs / fromIntegral (length xs)





focusIf :: IfExpr -> Focuser
focusIf ifexpr = FTrav $ \f focus -> if focus `passesIf` ifexpr
    then f focus
    else pure focus
  where
    passesIf :: Focus -> IfExpr -> Bool
    passesIf focus (IfAnd ifexprs) = all (passesIf focus) ifexprs
    passesIf focus (IfOr ifexprs) = any (passesIf focus) ifexprs
    passesIf focus (IfSingle comp) =
        let op = cmpOp comp
            q1 = fst $ cmpLHS comp
            q2 = fst $ cmpRHS comp
            f1s = evaluateEval focus $ snd $ cmpLHS comp
            f2s = evaluateEval focus $ snd $ cmpRHS comp
            results = [[applyOp op f1 f2 | f2 <- f2s] | f1 <- f1s]
        in case (q1, q2) of
            (QAll, QAll) -> all and results
            (QAll, QAny) -> all or results
            (QAny, QAll) -> any and results
            (QAny, QAny) -> any or results

    evaluateEval :: Focus -> Evaluatable -> [Either Rational Focus]
    evaluateEval focus eval = case eval of
        EText s               -> [Right $ FText s]
        ENumber n             -> [Left n]
        EFocuser (FTrav trav) -> Right <$> focus ^.. trav

    applyOp :: Oper -> Either Rational Focus -> Either Rational Focus -> Bool
    applyOp OpEq (Left n1) (Left n2) = n1 == n2
    applyOp OpEq (Right f1) (Right f2) = f1 == f2
    applyOp OpEq (Left n1) (Right (FText s2)) = Just n1 == readMaybeRational s2
    applyOp OpEq (Right (FText s1)) (Left n2) = readMaybeRational s1 == Just n2
    applyOp OpNe (Left n1) (Left n2) = n1 /= n2
    applyOp OpNe (Left n1) (Right (FText s2)) = case readMaybeRational s2 of
        Just n2 -> n1 /= n2
        Nothing -> False
    applyOp OpNe (Right (FText s1)) (Left n2) = case readMaybeRational s1 of
        Just n1 -> n1 /= n2
        Nothing -> False
    applyOp OpNe (Right f1) (Right f2) = f1 /= f2
    applyOp op e1 e2 = case op of
        OpLt -> applyOpOrd (<) e1 e2
        OpGt -> applyOpOrd (>) e1 e2
        OpLe -> applyOpOrd (<=) e1 e2
        OpGe -> applyOpOrd (>=) e1 e2
        _    -> False
      where
        applyOpOrd
            :: (forall a. Ord a => a -> a -> Bool)
            -> Either Rational Focus
            -> Either Rational Focus
            -> Bool
        applyOpOrd f (Left n1) (Left n2) = f n1 n2
        applyOpOrd f (Left n1) (Right (FText s2)) = case readMaybeRational s2 of
            Just n2 -> f n1 n2
            Nothing -> False
        applyOpOrd f (Right (FText s1)) (Left n2) = case readMaybeRational s1 of
            Just n1 -> f n1 n2
            Nothing -> False
        applyOpOrd f (Right (FText s1)) (Right (FText s2)) =
            case (readMaybeRational s1, readMaybeRational s2) of
                (Just n1, Just n2) -> f n1 n2
                _                  -> f s1 s2
        applyOpOrd _ _ _ = False

logicFocuser :: (Focus -> Bool) -> Focuser
logicFocuser pred = FTrav $ lens
    (\focus -> if pred focus
        then FText "1"
        else FText "0")
    const

focusIsUpper :: Focuser
focusIsUpper = logicFocuser (\case
    FText s -> T.all isUpper s
    _         -> False)

focusIsLower :: Focuser
focusIsLower = logicFocuser (\case
    FText s -> T.all isLower s
    _         -> False)

focusIsAlpha :: Focuser
focusIsAlpha = logicFocuser (\case
    FText s -> T.all isAlpha s
    _         -> False)

focusIsAlphaNum :: Focuser
focusIsAlphaNum = logicFocuser (\case
    FText s -> T.all isAlphaNum s
    _         -> False)

focusIsDigit :: Focuser
focusIsDigit = logicFocuser (\case
    FText s -> T.all isDigit s
    _         -> False)

focusIsSpace :: Focuser
focusIsSpace = logicFocuser (\case
    FText s -> T.all isSpace s
    _         -> False)

focusRegex :: Text -> Focuser
focusRegex regex = FTrav $ \f focus -> case focus of
    FText s ->
        let matchIdxs = getAllMatches (s =~ regex)
            (nonMatches, matches) = fromIndexes 0 s matchIdxs
            newMatches = map toTextUnsafe <$> traverse (f . FText) matches
        in  FText . T.concat . interleave nonMatches <$> newMatches
    _ -> pure focus

focusFilter :: IfExpr -> Focuser
focusFilter pred = focusCollect $ focusEach `composeFocusers` focusIf pred

focusContains :: Text -> Focuser
focusContains text = FTrav $ lens contains const
  where
    contains focus = case focus of
        FText s   -> FText $ if T.isInfixOf text s then "1" else "0"
        FList lst -> FText $ if any check lst then "1" else "0"
    check focus = case focus of
        FText s -> text == s
        _       -> False

focusStartsWith :: Text -> Focuser
focusStartsWith text = FTrav $ lens starts const
  where
    starts focus = case focus of
        FText s -> FText $ if T.isPrefixOf text s then "1" else "0"
        _       -> FText "0"

focusEndsWith :: Text -> Focuser
focusEndsWith text = FTrav $ lens ends const
  where
    ends focus = case focus of
        FText s -> FText $ if T.isSuffixOf text s then "1" else "0"
        _       -> FText "0"

focusLength :: Focuser
focusLength = FTrav $ \f focus -> case focus of
    fs@(FText s)     -> fs <$ f (FText . T.pack . show . T.length $ s)
    flst@(FList lst) -> flst <$ f (FText . T.pack . show . length $ lst)

parseListElemIdxs :: Parser [(Int, Int)]
parseListElemIdxs = do
    symbol "["
    idxs <- parseElemIdxs `sepBy` symbol ","
    symbol "]"
    pure idxs

parseElemIdxs :: Parser (Int, Int)
parseElemIdxs = lexeme $ do
    idx1 <- getOffset
    skipListElem
    idx2 <- getOffset
    pure (idx1, idx2 - idx1)

skipListElem :: Parser ()
skipListElem = choice
    [ inQuotes
    , inDoubleQuotes
    , inSquareBraces
    , inParens
    , inCurlyBraces
    , escapingCommaSquareBrace]
    >> void (optional (try $ ws >> skipListElem))

inQuotes = char '\'' >> escaping '\'' '\'' 1
inDoubleQuotes = char '"' >> escaping '"' '"' 1
inSquareBraces = char '[' >> escaping '[' ']' 1
inParens = char '(' >> escaping '(' ')' 1
inCurlyBraces = char '{' >> escaping '{' '}' 1
escapingCommaSquareBrace = void $ some $ satisfy (\c -> c /= ',' && c /= ']' && not (isSpace c))

escaping :: Char -> Char -> Int -> Parser ()
escaping start end depth = choice
    [ char end >> if depth == 1 then return () else void (optional $ escaping start end (depth - 1))
    , char start >> void (optional $ escaping start end (depth + 1))
    , char '\\' >> anySingle >> void (optional $ escaping start end depth)
    , anySingle >> void (optional $ escaping start end depth)
    ]

focusEl :: Focuser
focusEl = FTrav $ \f focus -> case focus of
    FText s -> case parseMaybe parseListElemIdxs s of
        Just idxs ->
            let (nonMatches, matches) = fromIndexes 0 s idxs
                newMatches = map toTextUnsafe <$> traverse (f . FText) matches
            in  FText . T.concat . interleave nonMatches <$> newMatches
        Nothing -> pure focus
    FList _ -> pure focus

parseObjKVIdxs :: Parser [((Int, Int), (Int, Int))]
parseObjKVIdxs = do
    symbol "{"
    idxs <- parseKVIdxs `sepBy` symbol ","
    symbol "}"
    pure idxs

parseKVIdxs :: Parser ((Int, Int), (Int, Int))
parseKVIdxs = do
    keyIdxs <- parseKeyIdxs
    symbol ":"
    valIdxs <- parseValIdxs
    pure (keyIdxs, valIdxs)

parseKeyIdxs :: Parser (Int, Int)
parseKeyIdxs = lexeme $ do
    idx1 <- getOffset
    skipKey
    idx2 <- getOffset
    pure (idx1, idx2 - idx1)

skipKey :: Parser ()
skipKey = choice
    [ inQuotes
    , inDoubleQuotes
    , inSquareBraces
    , inParens
    , inCurlyBraces
    , escapingColonCurlyBrace]
    >> void (optional (try $ ws >> skipKey))

escapingColonCurlyBrace = void $ some $ satisfy (\c -> c /= ':' && c /= '}' && not (isSpace c))

parseValIdxs :: Parser (Int, Int)
parseValIdxs = lexeme $ do
    idx1 <- getOffset
    skipVal
    idx2 <- getOffset
    pure (idx1, idx2 - idx1)

skipVal :: Parser ()
skipVal = choice
    [ inQuotes
    , inDoubleQuotes
    , inSquareBraces
    , inParens
    , inCurlyBraces
    , escapingCommaCurlyBrace]
    >> void (optional (try $ ws >> skipVal))

escapingCommaCurlyBrace = void $ some $ satisfy (\c -> c /= ',' && c /= '}' && not (isSpace c))

focusKV :: Focuser
focusKV = FTrav $ \f focus -> case focus of
    FText s -> case parseMaybe parseObjKVIdxs s of
        Just idxs ->
            let idxs_ = concatMap (\(a, b) -> [a, b]) idxs
                (nonMatches, matches) = fromIndexes 0 s idxs_
                matches_ = pairUp $ map FText matches
                newMatches_ = map toListUnsafe <$> traverse (f . FList) matches_
                newMatches = map toTextUnsafe . concat <$> newMatches_
            in  FText . T.concat . interleave nonMatches <$> newMatches
        Nothing -> pure focus
    FList _ -> pure focus
  where
    pairUp :: [a] -> [[a]]
    pairUp []             = []
    pairUp (a1 : a2 : as) = [a1, a2] : pairUp as
    pairUp _              = error "pairUp: list too short"

data KeyType
    = InQuotes Text
    | InDoubleQuotes Text
    | Default

focusKey :: Focuser
focusKey = FTrav $ \f focus -> case focus of
    FList [FText key, FText val] -> case stripKey key of
        InQuotes key_ -> setKey val . (\k -> "'" <> k <> "'") . toTextUnsafe <$> f (FText key_)
        InDoubleQuotes key_ -> setKey val . (\k -> "\"" <> k <> "\"") . toTextUnsafe <$> f (FText key_)
        Default -> setKey val . toTextUnsafe <$> f (FText key)
    FText _ ->
        let FTrav trav = focusKV `composeFocusers` focusKey
        in  trav f focus

stripKey :: Text -> KeyType
stripKey s
    | T.index s 0 == '"' && T.index s (T.length s - 1) == '"' =
        InDoubleQuotes $ T.drop 1 $ T.dropEnd 1 s
    | T.index s 0 == '\'' && T.index s (T.length s - 1) == '\''=
        InQuotes $ T.drop 1 $ T.dropEnd 1 s
    | otherwise = Default

setKey :: Text -> Text -> Focus
setKey val key = FList [FText key, FText val]

focusVal :: Focuser
focusVal = FTrav $ \f focus -> case focus of
    FList _ ->
        let FTrav trav = focusIndex 1
        in  trav f focus
    FText _ ->
        let FTrav trav = focusKV `composeFocusers` focusVal
        in  trav f focus

focusAtKey :: Text -> Focuser
focusAtKey key = focusKV
    `composeFocusers` focusIf (IfSingle $ Comparison (QAll, EFocuser focusKey) OpEq (QAll, EText key))
    `composeFocusers` focusVal

focusAtIdx :: Int -> Focuser
focusAtIdx i = focusCollect (focusEl) `composeFocusers` focusIndex i
