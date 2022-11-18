{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TodoTxt
  ( Todo(..)
  , State(..)
  , Part(..)
  , Priority, fromPriority, toPriority
  , ProjectName, fromProjectName, toProjectName
  , ContextName, fromContextName, toContextName
  , TagName, fromTagName, toTagName
  , TagValue, fromTagValue, toTagValue
  ) where

import Data.Char
import Data.Hashable
import Data.Text as T
import Data.Time.Calendar
import GHC.Generics (Generic)
import Data.Coerce

data Todo = Todo
  { todoState     :: !State
  , todoPriority  :: !(Maybe Priority)
  , todoTaskParts :: ![Part]
  } deriving (Eq, Ord, Show, Generic)

data State
  = Uncompleted !(Maybe Day)
    -- ^ optional creation date
  | Completed !(Maybe (Day, Maybe Day))
    -- ^ optional creation date with completion date (if any)
  deriving (Eq, Ord, Show)

newtype Priority = Priority { fromPriority :: Char }
  deriving newtype (Eq, Ord, Hashable, Show)
  deriving stock (Generic)

data Part
  = Text !Text
  | Project !ProjectName
  | Context !ContextName
  | Tag !TagName !TagValue
  deriving (Eq, Ord, Show, Generic)

instance Hashable Part

newtype TagName = TagName { fromTagName :: Text }
  deriving newtype (Eq, Ord, Hashable, Show)
  deriving stock (Generic)

newtype TagValue = TagValue { fromTagValue :: Text }
  deriving newtype (Eq, Ord, Hashable, Show)
  deriving stock (Generic)

newtype ProjectName = ProjectName { fromProjectName :: Text }
  deriving newtype (Eq, Ord, Hashable, Show)
  deriving stock (Generic)

newtype ContextName = ContextName { fromContextName :: Text }
  deriving newtype (Eq, Ord, Hashable, Show)
  deriving stock (Generic)

toPriority :: Char -> Either Text Priority
toPriority c
  | isAsciiUpper c = Right $ Priority c
  | otherwise =
    Left $ "Bad priority char \"" <> T.cons c "\". Should be A..Z"

toTagName :: Text -> Either Text TagName
toTagName = fromRawTag "tag name"

toTagValue :: Text -> Either Text TagValue
toTagValue = fromRawTag "tag value"

toProjectName :: Text -> Either Text ProjectName
toProjectName = fromNonWhitespace "project name"

toContextName :: Text -> Either Text ContextName
toContextName = fromNonWhitespace "context name"

{-# INLINE fromRawTag #-}
fromRawTag :: Coercible Text t => Text -> Text -> Either Text t
fromRawTag name t
  | not (T.any (\x -> isSpace x || x == ':') t) = Right $ coerce t
  | otherwise =
    Left $ "Bad " <> name <> "\"" <> t <>
    "\". Should not contain any whitespaces or colons."

{-# INLINE fromNonWhitespace #-}
fromNonWhitespace :: Coercible Text t => Text -> Text -> Either Text t
fromNonWhitespace name t
  | not (T.any isSpace t) = Right $ coerce t
  | otherwise =
    Left $ "Bad " <> name <> "\"" <> t <>
    "\". Should not contain any whitespaces."
