{-# LANGUAGE GADTs #-}
module Main where

import           Common           (readMaybeRational, showRational)
import           Data.Char        (isAlpha, isAlphaNum, isDigit, isLower,
                                   isSpace, isUpper, toLower, toUpper)
import           Data.Function    (on)
import           Data.List        (groupBy, transpose)
import           Data.List.Extra  (dropEnd, groupBy, sort, takeEnd, transpose)
import           Data.Maybe       (fromMaybe, mapMaybe, fromJust)
import qualified Data.Text        as T
import           Focusers         (interleave, myWords)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process   (readProcess)
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Text.Read        (readMaybe)

main :: IO ()
main = defaultMain $ testGroup "All" [focuserTests, mappingTests]

focuserTests :: TestTree
focuserTests = testGroup "Focuser Tests"
    [ testGroup "id"
        [ "id|get-tree" $= show [input]
        ]
    , testGroup "each"
        [ "each|get" $= concatMap (: "\n") input
        , "<words>.each|get" $=$ "words|get"
        ]
    , testGroup "words"
        [ "words|get-tree" $= show (words input)
        , "words|over id" $= input
        ]
    , testGroup "lines"
        [ "lines|get-tree" $= show (lines input)
        , "lines|over id" $= input
        ]
    , testGroup "ws"
        [ "ws|get-tree" $= show (getWS input)
        , "ws|over id" $= input
        ]
    , testGroup "cols"
        [ "cols|get-tree" $= show (getCols input)
        , "cols|over id" $= input
        ]
    , testGroup "slice"
        [ "{0}|get-tree" $= show [take 1 input]
        , "{2}|get-tree" $= show [take 1 $ drop 2 input]
        , "{1:3}|get-tree" $= show [take 2 $ drop 1 input]
        , "{:5}|get-tree" $= show [take 5 input]
        , "{7:}|get-tree" $= show [drop 7 input]
        , "{-2}|get-tree" $= show [take 1 $ takeEnd 2 input]
        , "{-3:}|get-tree" $= show [takeEnd 3 input]
        , "{:-5}|get-tree" $= show [dropEnd 5 input]
        , "{-10:-4}|get-tree" $= show [take 6 $ takeEnd 10 input]
        , "{-1000:5}|get-tree" $=$ "{:5}|get-tree"
        , "{10:1000}|get-tree" $=$ "{10:}|get-tree"
        , "{1:5,7:10}|get-tree" $= show [take 4 (drop 1 input) ++ take 3 (drop 7 input)]
        , "{}|get-tree" $= "[\"\"]"
        , "{:}|get-tree" $= show [input]
        ]
    , testGroup "sortLexBy"
        [ "<words>.sortedLexBy id.each|get-tree" $= show (sort $ words input)
        , "<words>.sortedLexBy id.each|over id" $= input
        ]
    , testGroup "minLexBy/maxLexBy/minLex/maxLex"
        [ "<words>.minLexBy id|get-tree" $=$ "<words>.sortedLexBy id.[0]|get-tree"
        , "<words>.maxLexBy id|get-tree" $=$ "<words>.sortedLexBy id.[-1]|get-tree"
        , "<words>.minLex|get-tree" $=$ "<words>.minLexBy id|get-tree"
        , "<words>.maxLex|get-tree" $=$ "<words>.maxLexBy id|get-tree"
        ]
    , testGroup "sortedBy"
        [ "<words.if isDigit>.sortedBy id.each|get-tree" $=
            show (map showRational $ sort $ map (fromJust . readMaybeRational . T.pack) $ filter (all isDigit) $ words input)
        , "<words.if isAlpha>.sortedBy id.each|over id" $=$ "id|over id"
        ]
    , testGroup "sorted"
        [ "<words>.sorted|get-tree" $=$ "<words>.sortedBy id|get-tree"
        ]
    , testGroup "minBy/maxBy/min/max"
        [ "<words>.minBy id|get-tree" $=$ "<words>.sortedBy id.[0]|get-tree"
        , "<words>.maxBy id|get-tree" $=$ "<words>.sortedBy id.[-1]|get-tree"
        , "<words>.min|get-tree" $=$ "<words>.minBy id|get-tree"
        , "<words>.max|get-tree" $=$ "<words>.maxBy id|get-tree"
        ]
    , testGroup "index"
        [ "[3]|get-tree" $= show [take 1 $ drop 3 input]
        , "[-3]|get-tree" $= show [take 1 $ takeEnd 3 input]
        , "[-1000]|get-tree" $= "[]"
        , "[1000]|get-tree" $= "[]"
        ]
    , testGroup "to"
        [ "to upper|get-tree" $= show [map toUpper input]
        , "to lower|get-tree" $= show [map toLower input]
        , "to reverse|get-tree" $= show [reverse input]
        ]
    , testGroup "len"
        [ "len|get-tree" $=$ "to len|get-tree"
        , "len|get-tree" $= show [show $ length input]
        ]
    , testGroup "()"
        [ "(words).len|get-tree" $=$ "words.len|get-tree"
        , "words.(len)|get-tree" $=$ "words.len|get-tree"
        , "(words.len)|get-tree" $=$ "words.len|get-tree"
        ]
    , testGroup "sum"
        [ "<words>.sum|get-tree" $= show [showRational (sum $ inputNums input)]
        , "words.sum|get-tree" $=
            show (map (showRational . sum . mapMaybe (readMaybeRational . T.pack . (:[]))) $ words input)
        ]
    , testGroup "product"
        [ "<words>.product|get-tree" $= show [showRational (product $ inputNums input)]
        , "words.product|get-tree" $=
            show (map (showRational . product . mapMaybe (readMaybeRational . T.pack . (:[]))) $ words input)
        ]
    , testGroup "average"
        [ "<words>.average|get-tree" $= show [showRational (average $ inputNums input)]
        , "words.average|get-tree" $=
            show (map (showRational . average . mapMaybe (readMaybeRational . T.pack . (:[]))) $ words input)
        ]
    , testGroup "add"
        [ "<words.add 1>.sum|get-tree" $= show [showRational (sum $ map (+1) $ inputNums input)]
        ]
    , testGroup "sub"
        [ "<words.sub 1>.sum|get-tree" $= show [showRational (sum $ map (subtract 1) $ inputNums input)]
        ]
    , testGroup "mult"
        [ "<words.mult 2>.sum|get-tree" $= show [showRational (sum $ map (*2) $ inputNums input)]
        ]
    , testGroup "div"
        [ "<words.div 2>.sum|get-tree" $= show [showRational (sum $ map (/ 2) $ inputNums input)]
        ]
    , testGroup "pow"
        [ "<words.pow 2>.sum|get-tree" $= show [showRational (sum $ map (^^ 2) $ inputNums input)]
        ]
    , testGroup "abs"
        [ "<words.abs>.sum|get-tree" $= show [showRational (sum $ map abs $ inputNums input)]
        ]
    , testGroup "sign"
        [ "<words.sign>.sum|get-tree" $= show [showRational (sum $ map signum $ inputNums input)]
        ]
    , testGroup "if"
        [ "words.if 1=1|get-tree" $= show (words input)
        , "words.if 1=2|get-tree" $= "[]"
        , "words.if \"1\"=\"1\"|get-tree" $= show (words input)
        , "words.if \"1\"=\"2\"|get-tree" $= "[]"
        , "words.if 1=\"1\"|get-tree" $= show (words input)
        , "words.if 1=\"2\"|get-tree" $= "[]"
        , "words.if \"1\"=1|get-tree" $= show (words input)
        , "words.if \"1\"=2|get-tree" $= "[]"
        , "words.if id<=id|get-tree" $= show (words input)
        , "words.if id=len|get-tree" $= show (filter (\w -> w == show (length w)) $ words input)
        , "words.if len=2|get-tree" $= show (filter (\w -> length w == 2) $ words input)
        , "words.if len=\"3\"|get-tree" $= show (filter (\w -> length w == 3) $ words input)
        , "<words>.if id<=id|get-tree" $= "[]"
        , "<words>.if id<id|get-tree" $= "[]"
        , "<words>.if id>id|get-tree" $= "[]"
        , "<words>.if id>=id|get-tree" $= "[]"
        , "words.if len<3 && len>1|get-tree" $= show (filter (\w -> length w < 3 && length w > 1) $ words input)
        , "words.if len<2 || len>2|get-tree" $= show (filter (\w -> length w < 2 || length w > 2) $ words input)
        , "words.if len>1 && len<3 || [0].isUpper=1|get-tree" $=
            show (filter (\w -> length w > 1 && length w < 3 || isUpper (head w)) $ words input)
        , "words.if [0].isUpper=1 || len>1 && len<3|get-tree" $=
            show (filter (\w -> isUpper (head w) || length w > 1 && length w < 3) $ words input)
        , "words.if ([0].isUpper=1 || len>1) && len<3|get-tree" $=
            show (filter (\w -> (isUpper (head w) || length w > 1) && length w < 3) $ words input)
        , "words.if len|get-tree" $= show (filter (\w -> length w == 1) $ words input)
        , "words.if all (each.isUpper)|get-tree" $= show (filter (all isUpper) $ words input)
        , "words.if any (each.isUpper)|get-tree" $= show (filter (any isUpper) $ words input)
        , "words.if 1=all each.isUpper|get-tree" $= show (filter (all isUpper) $ words input)
        , "words.if 1=any each.isUpper|get-tree" $= show (filter (any isUpper) $ words input)
        , "words.if each=each|get-tree" $= show (filter allEqual $ words input)
        , "words.if =\"ee\"|get-tree" $=$ "words.if id=\"ee\"|get-tree"
        ]
    , testGroup "isUpper"
        [ "words.if isUpper|get-tree" $= show (filter (all isUpper) $ words input)
        ]
    , testGroup "isLower"
        [ "words.if isLower|get-tree" $= show (filter (all isLower) $ words input)
        ]
    , testGroup "isAlpha"
        [ "words.if isAlpha|get-tree" $= show (filter (all isAlpha) $ words input)
        ]
    , testGroup "isAlphaNum"
        [ "words.if isAlphaNum|get-tree" $= show (filter (all isAlphaNum) $ words input)
        ]
    , testGroup "isSpace"
        [ "words.if isSpace|get-tree" $= show (filter (all isSpace) $ words input)
        ]
    , testGroup "isDigit"
        [ "words.if isDigit|get-tree" $= show (filter (all isDigit) $ words input)
        ]
    , testGroup "collect"
        [ "<words>|get-tree" $= show [words input]
        ]
    , testGroup "filter"
        [ "<words>.filter len<3.each|get-tree" $=$ "words.if len<3|get-tree" ]
    , testGroup "regex"
        [ "regex \"([a-z]|[A-Z]|[0-9]|_)+\"|get" $=$ "words|get"
        , "regex \"([a-z]|[A-Z]|[0-9]|_)+\"|over id" $=$ "id|over id"
        ]
    , testGroup "json"
        [ testGroup "kv"
            [ "kv.[0]|get" $$= "\"quiz\"\n"
            , "kv.[1].kv.[0]|get" $$= "\"sport\"\n\"maths\"\n"
            ]
        , testGroup "key"
            [ "key|get" $$=$ "kv.key|get"
            , "kv.key|get" $$=$ "kv.[0].{1:-1}|get"
            ]
        , testGroup "val"
            [ "val|get" $$=$ "kv.val|get"
            , "kv.val|get" $$=$ "kv.[1]|get"
            ]
        , testGroup "atKey"
            [ "atKey \"quiz\"|get" $$=$ "kv.if key=\"quiz\".val|get"
            ]
        , testGroup "atIdx"
            [ "atKey \"quiz\".atKey \"sport\".val.atKey \"options\".atIdx 3|get" $$= "\"Huston Rocket\"\n"
            ]
        , testGroup "el"
            [ "atKey \"quiz\".atKey \"sport\".val.atKey \"options\".<el>.[3]|get" $$= "\"Huston Rocket\"\n" ]
        ]
    ]

mappingTests :: TestTree
mappingTests = testGroup "Mapping Tests"
    [ testGroup "reverse"
        [ "id|over reverse" $= reverse input
        , "id|over reverse:reverse" $= input
        ]
    , testGroup "length"
        [ "id|over len" $= show (length input)
        ]
    , testGroup "map"
        [ "<words>|over map len" $= "1 2 3 3 1 1 1 1\n1 2 3 3 1 1 1 1  1\n1 2 3 3 1 1 1 1  1\n\n"
        , "words|over map upper" $=$ "words|over upper"
        , "id|over id" $= input
        ]
    , testGroup "append/prepend"
        [ "id|over append \"\"" $= input
        , "id|over prepend \"\"" $= input
        , "id|over append 1" $= (input ++ "1")
        , "id|over prepend 1" $= ("1" ++ input)
        , "id|over append \"hello\"" $= (input ++ "hello")
        , "id|over prepend \"hello\"" $= ("hello" ++ input)
        , "id|over append len" $= (input ++ show (length input))
        , "id|over prepend len" $= (show (length input) ++ input)
        , "id|over prepend <words>" $= input
        ]
    , testGroup "upper/lower"
        [ "id|over upper" $= map toUpper input
        , "id|over lower" $= map toLower input
        ]
    , testGroup "add/sub/div/pow/abs/sign"
        [ "id|over add 1" $= mapNums (+ 1) input
        , "id|over sub 1" $= mapNums (subtract 1) input
        , "id|over div 2" $= mapNums (/ 2) input
        , "id|over pow 2" $= mapNums (^^ 2) input
        , "id|over abs" $= mapNums abs input
        , "id|over sign" $= mapNums signum input
        ]
    , testGroup "slice"
        [ "id|over {0}" $= take 1 input
        , "id|over {2}" $= take 1 (drop 2 input)
        , "id|over {1:3}" $= take 2 ( drop 1 input)
        , "id|over {:5}" $= take 5 input
        , "id|over {7:}" $= drop 7 input
        , "id|over {-2}" $= take 1 ( takeEnd 2 input)
        , "id|over {-3:}" $= takeEnd 3 input
        , "id|over {:-5}" $= dropEnd 5 input
        , "id|over {-10:-4}" $= take 6 ( takeEnd 10 input)
        , "id|over {-1000:5}" $=$ "id|over {:5}"
        , "id|over {10:1000}" $=$ "id|over {10:}"
        , "id|over {1:5,7:10}" $= take 4 (drop 1 input) ++ take 3 (drop 7 input)
        , "id|over {}" $= ""
        , "id|over {:}" $= input
        ]
    , testGroup "sortLexBy"
        [ "<words>|over sortLexBy id" $= "1 1 2 2 3 3 Ccc Hhh\nMmm _ _ _ _ _ a bb  dd1\ne f gg ii2 j k ll nn3  o\n\n"
        , "<words.<each>>|over sortLexBy id" $= input
        ]
    , testGroup "sortLex"
        [ "<words>|over sortLexBy id" $=$ "<words>|over sortLex"
        ]
    , testGroup "sortBy"
        [ "<words>|over sortBy id" $= "a bb Ccc dd1 e 1 1 3\n_ f gg Hhh ii2 j 2 2  _\n_ k ll Mmm nn3 o 3 _  _\n\n"
        , "<words.<each>>|over sortBy id" $= input
        ]
    , testGroup "sort"
        [ "<words>|over sortBy id" $=$ "<words>|over sort"
        ]
    , testGroup "id"
        [ "<words>|over id" $= input
        ]
    , testGroup "to"
        [ "words|over to len" $=$ "words|over len" ]
    ]

allEqual :: (Eq a) => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

anyEqual :: (Eq a) => [a] -> Bool
anyEqual []     = False
anyEqual (x:xs) = x `elem` xs || anyEqual xs

mapNums :: (Rational -> Rational) -> String -> String
mapNums f str =
    let (ws, words) = myWords $ T.pack str
        new_words = map (mapNum f . T.unpack) words
    in  T.unpack $ T.concat $ interleave ws words
  where
    mapNum :: (Rational -> Rational) -> String -> String
    mapNum f str = case readMaybe str of
        Nothing -> str
        Just n  -> T.unpack $ showRational $ f n

getWS :: String -> [String]
getWS str =
    let groups = groupBy (\c1 c2 -> isSpace c1 == isSpace c2) str
    in  filter (isSpace . head) groups

getCols :: String -> [[String]]
getCols = transpose . map words . lines

inputNums :: String -> [Rational]
inputNums str = mapMaybe (readMaybeRational . T.pack) $ words str

average :: [Rational] -> Rational
average [] = 0
average ns = sum ns / fromIntegral (length ns)

{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "test/input.txt"

{-# NOINLINE json #-}
json :: String
json = unsafePerformIO $ readFile "test/input.json"

infixl 1 $=
($=) :: String -> String -> TestTree
($=) command desiredOutput = testCase command $ do
    output <- readProcess "./smh" [command] input
    output @?= desiredOutput

infixl 1 $=$
($=$) :: String -> String -> TestTree
($=$) cmd1 cmd2 = testCase (cmd1 ++ " == " ++ cmd2) $ do
    out1 <- readProcess "./smh" [cmd1] input
    out2 <- readProcess "./smh" [cmd2] input
    out1 @?= out2

-- This is stupid, but whatever
infixl 1 $$=
($$=) :: String -> String -> TestTree
($$=) command desiredOutput = testCase command $ do
    output <- readProcess "./smh" [command] json
    output @?= desiredOutput

infixl 1 $$=$
($$=$) :: String -> String -> TestTree
($$=$) cmd1 cmd2 = testCase (cmd1 ++ " == " ++ cmd2) $ do
    out1 <- readProcess "./smh" [cmd1] json
    out2 <- readProcess "./smh" [cmd2] json
    out1 @?= out2

