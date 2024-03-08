{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE KindSignatures #-}

module Common(module Common) where

import           Control.Applicative        (empty, (<|>))
import           Control.Lens               (Lens', Traversal', lens, (^..))
import           Control.Loop               (numLoop)
import           Control.Monad              (forM_)
import           Control.Monad.ST.Strict    (ST, runST)
import           Data.Array.ST              (STUArray)
import qualified Data.Array.ST              as A
import           Data.Data                  (Data)
import           Data.List                  (nub, sort)
import           Data.List.Extra            (nubOrd)
import           Data.Ratio                 (denominator, numerator)
import           Data.STRef                 ()
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, label)
import           Text.Megaparsec.Char       (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Read                  (readMaybe)

data Focus
    = FText !Text
    | FList ![Focus]
  deriving Data

instance Show Focus where
    show (FText str) = show str
    show (FList lst) = show lst

instance Eq Focus where
    (FText str1) == (FText str2) = str1 == str2
    (FList lst1) == (FList lst2) = lst1 == lst2
    _ == _                       = False

toTextUnsafe :: Focus -> Text
toTextUnsafe (FText str) = str
toTextUnsafe _           = error "toText called on a non-FText"

toListUnsafe :: Focus -> [Focus]
toListUnsafe (FList lst) = lst
toListUnsafe _           = error "toList called on a non-FText"

_toListUnsafe :: Lens' [Focus] Focus
_toListUnsafe = lens FList $ const toListUnsafe

newtype Focuser = FTrav (Traversal' Focus Focus)

composeFocusers :: Focuser -> Focuser -> Focuser
composeFocusers (FTrav a) (FTrav b) = FTrav (a . b)

foldFocusers :: [Focuser] -> Focuser
foldFocusers = foldr composeFocusers (FTrav id)

type Action = Text -> Focuser -> IO ()

type Mapping = Focus -> Focus

foldMappings :: [Mapping] -> Mapping
foldMappings = foldr (flip (.)) id

type Parser = Parsec Void Text

showRational :: Rational -> Text
showRational n = if denominator n == 1
    then T.pack $ show (numerator n)
    else T.pack $ show (fromInteger (numerator n) / fromInteger (denominator n))

data Range
    = RangeSingle !Int
    | RangeRange !(Maybe Int) !(Maybe Int)

getIndexes :: [Range] -> Int -> [Int]
getIndexes ranges len = nubOrd . sort . concatMap (getIndexes' . fixRange) $ ranges
  where
    getIndexes' (RangeSingle i) = [i]
    getIndexes' (RangeRange mstart mend) =
        case (mstart, mend) of
            (Just start, Just end) -> [start .. end - 1]
            (Just start, Nothing)  -> [start .. len - 1]
            (Nothing, Just end)    -> [0 .. end - 1]
            (Nothing, Nothing)     -> [0 .. len - 1]

    fixRange (RangeSingle i) = RangeSingle (fixIndex i)
    fixRange (RangeRange mstart mend) = RangeRange
        (fixIndex <$> mstart) (fixIndex <$> mend)

    fixIndex i
        | i < 0 = max (i + len) 0
        | otherwise = min i len


data Evaluatable
    = EText !Text
    | ENumber !Rational
    | EFocuser { evalFocuserUnsafe :: !Focuser }

data IfExpr
    = IfAnd ![IfExpr]
    | IfOr ![IfExpr]
    | IfSingle !Comparison

data Comparison = Comparison
    { cmpLHS :: !(Quantor, Evaluatable)
    , cmpOp  :: !Oper
    , cmpRHS :: !(Quantor, Evaluatable)
    }

data Quantor = QAll | QAny

data Oper
    = OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe

ws :: Parser ()
ws = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol ws

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

integer :: Parser Int
integer = label "integer" $ lexeme $ L.signed ws L.decimal

rational :: Parser Rational
rational = toRational <$> label "number" (lexeme $ L.signed ws L.scientific)

mapText :: (Char -> a) -> Text -> [a]
mapText f = T.foldr (\c cs -> f c : cs) []

unsort :: [Int] -> Int -> [Int]
unsort is isLen = runST $ do
    is' <- A.newListArray (0, isLen - 1) is :: ST s (STUArray s Int Int)
    is'' <- A.newArray (0, isLen - 1) 0 :: ST s (STUArray s Int Int)
    numLoop 0 (isLen - 1) $ \i -> do
        j <- A.readArray is' i
        A.writeArray is'' j i
    return []

makeFilteredText :: Int -> [Int] -> Text -> Text
makeFilteredText maxLen is str = T.unfoldrN maxLen builder (0, is)
  where
    builder :: (Int, [Int]) -> Maybe (Char, (Int, [Int]))
    builder (_, [])     = Nothing
    builder (n, i : is) = Just (T.index str i, (n + 1, is))

focusTo :: Mapping -> Focuser
focusTo mapping = FTrav $ lens mapping const

mappingTo :: Focuser -> Mapping
mappingTo (FTrav trav) focus = case (focus, focus ^.. trav) of
    (FText _, [FText str]) -> FText str
    _                      -> focus

fromIndexes :: Int -> Text -> [(Int, Int)] -> ([Text], [Text])
fromIndexes _ str [] = ([str], [])
fromIndexes offset str ((i, j) : is) =
    let (nonMatch, T.splitAt j -> (match, str')) = T.splitAt (i - offset) str
        (nonMatches, matches) = fromIndexes (i + j) str' is
    in  (nonMatch : nonMatches, match : matches)

readMaybeRational :: Text -> Maybe Rational
readMaybeRational s = (toRational <$> readMaybeInteger s) <|> (toRational <$> readMaybeDouble s)

readMaybeInteger :: Text -> Maybe Integer
readMaybeInteger = readMaybe . T.unpack

readMaybeDouble :: Text -> Maybe Double
readMaybeDouble = readMaybe . T.unpack
