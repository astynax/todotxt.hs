{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
import Test.HUnit
import Test.HUnit.Text (runTestTTAndExit)
import Text.Megaparsec

import TodoTxt
import TodoTxt.Parser (todoP)
import Data.Time.Calendar (fromGregorian)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ TestLabel "checkbox" checkboxTest
  , TestLabel "dates" datesTest
  , TestLabel "priorities" prioritiesTest
  , TestLabel "parts" partsTest
  ]

checkboxTest :: Test
checkboxTest = TestCase do
  assertParsingSucceed "minimal task"
    (Todo (Uncompleted Nothing) Nothing [Text "foo"])
    "foo"
  assertParsingSucceed "incomplete task with spaces in front"
    (Todo (Uncompleted Nothing) Nothing [Text "x", Text "foo"])
    "  x foo"
  assertParsingSucceed "minimal completed task"
    (Todo (Completed Nothing) Nothing [Text "foo"])
    "x foo"
  assertParsingSucceed "uncompleted task with x in name"
    (Todo (Uncompleted Nothing) Nothing [Text "xfoo"])
    "xfoo"
  assertParsingSucceed "uncompleted task with X as first word"
    (Todo (Uncompleted Nothing) Nothing [Text "X", Text "foo"])
    "X foo"

datesTest :: Test
datesTest = TestCase do
  assertParsingSucceed "uncomplete task with date"
    (Todo (Uncompleted . Just $ fromGregorian 2022 11 18) Nothing [Text "foo"])
    "2022-11-18 foo"
  assertDates "only creation date"
    (2001, 2, 3) Nothing "foo"
    "x 2001-02-03 foo"
  assertDates "only creation date even if there are more dates in body"
    (2001, 2, 3) Nothing "x2001-02-03"
    "x 2001-02-03 x2001-02-03"
  assertDates "both dates"
    (2001, 2, 1) (Just (2001, 2, 3)) "bar"
    "x 2001-02-03 2001-02-01 bar"
  where
    assertDates msg d1 d2 txt =
      assertEqual msg (Just $ mkTodo d1 d2 txt) . parseMaybe todoP
    mkTodo completed created txt =
      Todo (Completed $ Just (f completed, f <$> created)) Nothing [Text txt]
      where
        f (y, m, d) = fromGregorian y m d

prioritiesTest :: Test
prioritiesTest = TestCase do
  assertParsingSucceed "uncomplete task with priority"
    (Todo (Uncompleted Nothing) (prio 'A') [Text "foo"])
    "(A) foo"
  assertParsingSucceed "complete task with priority"
    (Todo (Completed Nothing) (prio 'Z') [Text "bar"])
    "x (Z) bar"
  where
    prio c = case toPriority c of
      Left e  -> error $ unpack e
      Right p -> Just p

partsTest :: Test
partsTest = TestCase do
  assertParts "single plain text part"
    [Text "foo"]
    "foo"
  assertParts "very spaced text"
    [Text "foo", Text "bar"]
    "  foo   bar  "
  assertParts "complex body"
    [ Text "foo", prj "p1", "a" =: "b", ctx "c1"
    , "k" =: "v", Text "bar", prj "p2"]
    "  foo +p1 a:b  \t@c1   k:v bar +p2   "
  where
    assertParts msg ps =
      assertParsingSucceed msg (Todo (Uncompleted Nothing) Nothing ps)
    prj = Project . fromRight . toProjectName
    ctx = Context . fromRight . toContextName
    k =: v = Tag (fromRight $ toTagName k) (fromRight $ toTagValue v)

fromRight = either (error . unpack) id

assertParsingSucceed :: String -> Todo -> Text -> Assertion
assertParsingSucceed msg todo =
  assertEqual msg (Just todo) . parseMaybe todoP
