{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}

module Common(module Common) where

import           Control.Applicative        (empty)
import           Control.Lens               (Lens', Traversal', lens)
import           Control.Loop               (numLoop)
import           Control.Monad              (forM_)
import           Control.Monad.ST.Strict    (ST, runST)
import           Data.Array.ST              (STUArray)
import qualified Data.Array.ST              as A
import           Data.Data                  (Data)
import           Data.List                  (nub, sort)
import           Data.List.Extra            (nubOrd)
import           Data.Scientific            (Scientific, floatingOrInteger,
                                             fromFloatDigits, toRealFloat)
import           Data.STRef                 ()
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, label)
import           Text.Megaparsec.Char       (space1)
import qualified Text.Megaparsec.Char.Lexer as L

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

showScientific :: Scientific -> Text
showScientific n = case floatingOrInteger n :: Either Double Int of
    Left d  -> T.pack $ show d
    Right i -> T.pack $ show i

safeDiv :: Scientific -> Scientific -> Scientific
safeDiv a b = fromFloatDigits (toRealFloat a / toRealFloat b)

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
    | ENumber !Scientific
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

scient :: Parser Scientific
scient = label "number" $ lexeme $ L.signed ws L.scientific

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

