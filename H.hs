module H where

import Control.Applicative
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Format (left)
import Data.Text.Buildable (Buildable)
import Data.Time
import Debug.Trace
import Safe
import Text.Parsec (modifyState)
import Text.Parsec.String
import Text.ParserCombinators.Parsec as P hiding ((<|>), many)

data Task = Task
    { description :: String
    , tags :: [String]
    , context :: Maybe String
    , isComplete :: Bool
    , estimated :: Maybe DiffTime
    , spent :: Maybe DiffTime
    , started :: Maybe LocalTime
    } deriving (Eq, Show)

class Encode a where
    encode :: a -> String

instance Encode Task where
    encode t = (if isComplete t then "x " else "")
        ++ maybe "" (\c -> '@':c ++ " ") (context t)
        ++ description t
        ++ " "
        ++ concat (fmap (\tg -> '+' : tg ++ " ") (tags t))
        ++ maybe "" (\s -> '(' : encode s
            ++ maybe "" (\sp -> ',' : encode sp) (spent t)
            ++ ") ") (estimated t)
        ++ maybe "" (\s -> '{' : encode s ++ "}") (started t)

instance Encode DiffTime where
    encode 0 = ""
    encode tod = encode . timeToTimeOfDay $ tod

instance Encode TimeOfDay where
    encode (TimeOfDay 0 0 _) = ""
    encode tod = show (todHour tod) ++ ":" ++ twoDigit (todMin tod)

instance Encode Day where
    encode =
        (\(y, m, d) -> L.intercalate "/"
            [twoDigit m, twoDigit d, show y])
        . toGregorian

instance Encode LocalTime where
    encode lt = encode (localDay lt)
        ++ case encode (localTimeOfDay lt) of
            "" -> ""
            tod -> ' ' : tod

twoDigit :: Buildable a => a -> String
twoDigit = LT.unpack . toLazyText . left 2 '0'

emptyTask :: Task
emptyTask = Task "" [] Nothing False Nothing Nothing Nothing

task :: String -> Task
task d = emptyTask { description = d }

describe :: String -> Task -> Task
describe d t = t { description = description t ++ d }

tag :: String -> Task -> Task
tag tg tk = tk { tags = tg : tags tk }

contextualize :: String -> Task -> Task
contextualize c t = t { context = Just c }

complete :: Task -> Task
complete t = t { isComplete = True }

estimate :: DiffTime -> Task -> Task
estimate tm tk = tk { estimated = Just tm }

spend :: DiffTime -> Task -> Task
spend tm tk = tk { spent = Just tm }

start :: LocalTime -> Task -> Task
start tm tk = tk { started = Just tm }

days :: Int -> DiffTime
days n = timeOfDayToTime $ TimeOfDay (n * 24) 0 0

hours :: Int -> DiffTime
hours n = timeOfDayToTime $ TimeOfDay n 0 0

minutes :: Int -> DiffTime
minutes n = timeOfDayToTime $ TimeOfDay 0 n 0

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

normalize :: Task -> Task
normalize t = t { description = strip . description $ t }

strip :: String -> String
strip = T.unpack . T.strip . T.pack
