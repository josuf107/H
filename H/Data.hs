module H.Data where

import Control.Applicative
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Format (left)
import Data.Text.Buildable (Buildable)
import Data.Time
import Test.QuickCheck

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

hours :: Int -> DiffTime
hours n = timeOfDayToTime $ TimeOfDay n 0 0

minutes :: Int -> DiffTime
minutes n = timeOfDayToTime $ TimeOfDay 0 n 0

normalize :: Task -> Task
normalize t = t { description = strip . description $ t }

strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- Visible for testing only (here to avoid orphans)
instance Arbitrary Task where
    arbitrary = (start <$> aLocalTime)
        <.> (spend <$> aDiffTime)
        <.> (estimate <$> aDiffTime)
        <.> (contextualize <$> nonEmptyAlphaNums)
        <.> (tag <$> nonEmptyAlphaNums)
        <.> (describe <$> nonEmptyAlphaNums)
        <*> pure emptyTask

nonEmptyAlphaNums :: Gen String
nonEmptyAlphaNums = (getNonEmpty <$> arbitrary)
    `suchThat` all isAlphaNum

isAlphaNum :: Char -> Bool
isAlphaNum = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

aDiffTime :: Gen DiffTime
aDiffTime =
    ((+) . hours
        <$> (getNonNegative <$> arbitrary `suchThat` (<24)))
    <*> (minutes
        <$> (getNonNegative <$> arbitrary `suchThat` (<60)))

aTimeOfDay :: Gen TimeOfDay
aTimeOfDay = timeToTimeOfDay <$> aDiffTime

aLocalTime :: Gen LocalTime
aLocalTime = LocalTime
    <$> (fromGregorian 2014
        <$> (getNonNegative <$> arbitrary)
        <*> (getNonNegative <$> arbitrary))
    <*> aTimeOfDay

(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
f <.> g = ((.) <$> f) <*> g
