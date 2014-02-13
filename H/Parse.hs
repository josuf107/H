module H.Parse where

import H.Data

import Control.Applicative
import qualified Data.Text as T
import Data.Time
import Text.Parsec (modifyState)
import Text.Parsec.String
import Text.ParserCombinators.Parsec as P hiding ((<|>), many)

parseTask :: String -> Either ParseError Task
parseTask = P.runParser taskParser emptyTask "task"

modifyAndReturn :: (a -> a) -> GenParser Char a a
modifyAndReturn f = do
    modifyState f
    getState

taskParser :: GenParser Char Task Task
taskParser = do
    P.optional (try doneParser)
    many (choice
        [ tagParser
        , contextParser
        , timesParser
        , startParser
        , descriptionParser
        ])
    modifyAndReturn normalize

descriptionParser :: GenParser Char Task Task
descriptionParser = do
    d <- P.many1 (alphaNum <|> space)
    modifyAndReturn (describe d)

doneParser :: GenParser Char Task ()
doneParser = do
    char 'x' >> space
    modifyState complete

contextParser :: GenParser Char Task Task
contextParser = do
    char '@'
    c <- P.many1 alphaNum
    spaces
    modifyAndReturn (contextualize c)

tagParser :: GenParser Char Task Task
tagParser = do
    char '+'
    t <- P.many1 alphaNum
    spaces
    modifyAndReturn (tag t)

startParser :: GenParser Char Task Task
startParser = do
    char '{'
    spaces
    mon <- read <$> many1 digit
    char '/'
    d <-  read <$> many1 digit
    char '/'
    y <-  read <$> many1 digit
    spaces
    ts <- choice [timeString, return 0]
    spaces
    char '}'
    spaces
    modifyAndReturn (start (lt mon d y ts))
    where
        lt :: Int -> Int -> Integer -> DiffTime -> LocalTime
        lt mon d y ts = LocalTime
            (fromGregorian y mon d)
            (timeToTimeOfDay ts)

timesParser :: GenParser Char Task Task
timesParser = do
    char '('
    ts <- timeString
    spaces
    P.optional $ do
        char ','
        ss <- timeString
        modifyState (spend ss)
    spaces
    char ')'
    spaces
    modifyAndReturn (estimate ts)

timeString :: GenParser Char Task DiffTime
timeString =
    sum <$> many1 (spaces >> choice (fmap try
            [ minuteParser
            , hourParser
            , hourMinuteParser
            , dayParser
            ]))

hourMinuteParser :: GenParser Char Task DiffTime
hourMinuteParser = do
    h <- read <$> many1 digit
    char ':'
    m <- read <$> many1 digit
    return (hours h + minutes m)

shortTimeParser :: (Int -> DiffTime) -> Char -> GenParser Char Task DiffTime
shortTimeParser f c = do
    n <- many1 digit
    char c
    return . f . read $ n

minuteParser :: GenParser Char Task DiffTime
minuteParser = shortTimeParser minutes 'm'

hourParser :: GenParser Char Task DiffTime
hourParser = shortTimeParser hours 'h'

dayParser :: GenParser Char Task DiffTime
dayParser = shortTimeParser days 'd'
