module TodoTxt.Parser where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (isSpace, isDigit)
import Data.Text
import Data.Time.Calendar

import TodoTxt

newtype ValidationError = ValidationError Text
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ValidationError where
  showErrorComponent (ValidationError t) = unpack t

type Parser = Parsec ValidationError Text

todoP :: Parser Todo
todoP = do
  completed <- checkboxP
  prio <- optional $ priorityP <* hspace1
  state <- if completed then completedP else uncompletedP
  Todo state prio <$> hspaced partP <* eof
  where
    uncompletedP = Uncompleted <$> optional (dayP <* hspace1)
    completedP = Completed <$> optional do
      d <- dayP <* hspace1
      optional (dayP <* hspace1) >>= \case
        Nothing -> pure (d, Nothing)
        Just d2 -> pure (d2, Just d)

checkboxP :: Parser Bool
checkboxP = True <$ try (char 'x' <* hspace1) <|> pure False

dayP :: Parser Day
dayP =
  fromGregorian
  <$> d 4
  <* char '-'
  <*> d 2
  <* char '-'
  <*> d 2
  where
    d n = lookAhead (digitsExactly n) *> decimal

partP :: Parser Part
partP =
  Project <$> projectNameP
  <|> Context <$> contextNameP
  <|> try (uncurry Tag <$> tagP)
  <|> Text <$> nonSpace1

priorityP :: Parser Priority
priorityP = between (char '(') (char ')') $ using toPriority upperChar

projectNameP :: Parser ProjectName
projectNameP = char '+' *> using toProjectName nonSpace1

contextNameP :: Parser ContextName
contextNameP = char '@' *> using toContextName nonSpace1

tagP :: Parser (TagName, TagValue)
tagP = (,) <$> tagNameP <*> (satisfy (== ':') *> tagValueP)

tagNameP :: Parser TagName
tagNameP = using toTagName nonSpaceOrColon1

tagValueP :: Parser TagValue
tagValueP = using toTagValue nonSpaceOrColon1

using :: (a -> Either Text b) -> Parser a -> Parser b
using validate = (handle . validate =<<)
  where
    handle = \case
      Left e  -> customFailure $ ValidationError e
      Right v -> pure v

nonSpace1 :: Parser Text
nonSpace1 = takeWhile1P (Just "non-space") (not . isSpace)

nonSpaceOrColon1 :: Parser Text
nonSpaceOrColon1 = takeWhile1P Nothing \x -> not (isSpace x) && x /= ':'

digitsExactly :: Int -> Parser ()
digitsExactly 0 = pure ()
digitsExactly n = satisfy isDigit *> digitsExactly (n - 1)

hspaced :: Parser a -> Parser [a]
hspaced p = hspace *> go
  where
    go = (:) <$> p <*> (try (hspace *> go) <|> try ([] <$ hspace) <|> pure [])
