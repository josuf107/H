module H where

import Control.Applicative
import qualified Data.Text as T
import Safe
import Text.Parsec (modifyState)
import Text.Parsec.String
import Text.ParserCombinators.Parsec as P hiding ((<|>), many)

data Task = Task
    { description :: String
    , tags :: [String]
    } deriving (Eq, Show)

emptyTask :: Task
emptyTask = Task "" []

task :: String -> Task
task d = emptyTask { description = d }

describe :: String -> Task -> Task
describe d t = t { description = description t ++ d }

tag :: String -> Task -> Task
tag tg tk = tk { tags = tg : tags tk }

parseTask :: String -> Either ParseError Task
parseTask = P.runParser taskParser emptyTask "task"

taskParser :: GenParser Char Task Task
taskParser = do
    many (choice
        [ tagParser
        , descriptionParser
        ])
    modifyState stripTask
    getState

descriptionParser :: GenParser Char Task Task
descriptionParser = do
    d <- P.many1 (alphaNum <|> space)
    modifyState (describe d)
    getState

tagParser :: GenParser Char Task Task
tagParser = do
    spaces >> char '+'
    t <- P.many1 alphaNum
    spaces
    modifyState (tag t)
    getState

stripTask :: Task -> Task
stripTask t = t { description = strip . description $ t }

strip :: String -> String
strip = T.unpack . T.strip . T.pack
