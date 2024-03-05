{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Mappings where

import           Common          (Evaluatable (..), Focus (FList, FText),
                                  Focuser (..), Mapping, Range, getIndexes,
                                  makeFilteredText, mapText,
                                  showRational, toTextUnsafe, readMaybeRational)
import           Control.Lens    ((^..))
import           Data.Char       (toLower, toUpper)
import           Data.Function   (on)
import           Data.List       (sortBy)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Read       (readMaybe)

mappingReverse :: Mapping
mappingReverse (FList lst) = FList (reverse lst)
mappingReverse (FText str) = FText (T.reverse str)

mappingLength :: Mapping
mappingLength (FText str) = FText $ T.pack $ show $ T.length str
mappingLength flist       = flist

mappingMap :: Mapping -> Mapping
mappingMap mapping (FList lst) = FList $ map mapping lst
mappingMap mapping (FText str) = FText $ T.concat $ mapText
    (toTextUnsafe . mapping . FText . T.singleton) str

mappingAppend :: Evaluatable -> Mapping
mappingAppend (EText str') (FText str) = FText $ T.append str str'
mappingAppend (ENumber n) (FText str) = FText $ T.append str (showRational n)
mappingAppend (EFocuser (FTrav trav)) fstr@(FText str) = case fstr ^.. trav of
    [FText s] -> FText $ T.append str s
    _         -> fstr
mappingAppend _ flist            = flist

mappingPrepend :: Evaluatable -> Mapping
mappingPrepend (EText str') (FText str) = FText $ T.append str' str
mappingPrepend (ENumber n) (FText str) = FText $ T.append (showRational n) str
mappingPrepend (EFocuser (FTrav trav)) fstr@(FText str) = case fstr ^.. trav of
    [FText s] -> FText $ T.append s str
    _         -> fstr
mappingPrepend _ flist            = flist

mappingUpper :: Mapping
mappingUpper (FText str) = FText $ T.toUpper str
mappingUpper flist       = flist

mappingLower :: Mapping
mappingLower (FText str) = FText $ T.toLower str
mappingLower flist       = flist

mappingMath :: (Rational -> Rational) -> Mapping
mappingMath f (FText str) = case readMaybeRational str of
    Nothing -> FText str
    Just n  -> FText $ showRational $ f n
mappingMath _ flist         = flist

mappingAdd :: Rational -> Mapping
mappingAdd = mappingMath . (+)

mappingSub :: Rational -> Mapping
mappingSub = mappingMath . flip (-)

mappingMult :: Rational -> Mapping
mappingMult = mappingMath . (*)

mappingDiv :: Rational -> Mapping
mappingDiv = mappingMath . flip (/)

mappingPow :: Int -> Mapping
mappingPow = mappingMath . flip (^^)

mappingAbs :: Mapping
mappingAbs = mappingMath abs

mappingSign :: Mapping
mappingSign = mappingMath signum

mappingSlice :: [Range] -> Mapping
mappingSlice ranges (FText str) = FText filtered_str
  where
    str_length = T.length str
    is = getIndexes ranges str_length
    filtered_str = makeFilteredText str_length is str

mappingSlice _ flist = flist

mappingSortBy :: Focuser -> Mapping
mappingSortBy (FTrav trav) focus = case focus of
    FText str -> FText $ T.pack $ sortBy (cmp `on` (FText . T.singleton)) $ T.unpack str
    FList lst   -> FList $ sortBy cmp lst
  where
    cmp f1 f2 =
        let f1' = f1 ^.. trav
            f2' = f2 ^.. trav
        in case (f1', f2') of
            ([FText s1], [FText s2]) -> case (readMDouble s1, readMDouble s2) of
                (Just n1, Just n2) -> compare n1 n2
                _                  -> EQ
            _ -> EQ

    readMDouble :: Text -> Maybe Double
    readMDouble = readMaybe . T.unpack

mappingSortLexBy :: Focuser -> Mapping
mappingSortLexBy (FTrav trav) focus = case focus of
    FText str -> FText $ T.pack $ sortBy (cmp `on` (FText . T.singleton)) $ T.unpack str
    FList lst   -> FList $ sortBy cmp lst
  where
    cmp f1 f2 =
        let f1' = f1 ^.. trav
            f2' = f2 ^.. trav
        in case (f1', f2') of
            ([FText s1], [FText s2]) -> compare s1 s2
            _                        -> EQ

mappingId :: Mapping
mappingId = id
