module H.Test where

import H

import Data.Either
import Data.Time
import Test.HUnit

runTests :: IO Counts
runTests = runTestTT enabledTests

enabledTests :: Test
enabledTests = TestList
    [ parseTests
    ]

parseTests :: Test
parseTests = TestList
    [ testSimple
    , testEmpty
    , testTagged
    , testMultiTagged
    , testContexted
    , testCompleted
    , testNotCompleted
    , testEstimated
    , testTimed
    , testTimed2
    , testTimed3
    , testTimed4
    , testStarted
    ]

rightOrFail :: (Show a, Show b, Eq b) => Either a b -> b -> Test
rightOrFail (Right x) y = x ~?= y
rightOrFail (Left e) _ = TestCase (assertFailure . show $ e)

notYet :: Test
notYet = TestCase (assertFailure "not yet tested")

testSimple :: Test
testSimple = rightOrFail (parseTask "do this") (task "do this")

testEmpty :: Test
testEmpty = rightOrFail (parseTask "") (task "")

testTagged :: Test
testTagged = rightOrFail (parseTask "do +tag this")
    (tag "tag" . task $ "do this")

testMultiTagged :: Test
testMultiTagged = rightOrFail (parseTask "do +tag this +tag2")
    (tag "tag2" . tag "tag" . task $ "do this")

testContexted :: Test
testContexted = rightOrFail (parseTask "do @TEST this")
    (contextualize "TEST" . task $ "do this")

testCompleted :: Test
testCompleted = rightOrFail (parseTask "x do this")
    (complete . task $ "do this")

testNotCompleted :: Test
testNotCompleted = rightOrFail (parseTask "xdo this")
    (task "xdo this")

testEstimated :: Test
testEstimated = rightOrFail (parseTask "do this (2h)")
    (estimate (hours 2) . task $ "do this")

testTimed :: Test
testTimed = rightOrFail (parseTask "do this (2h,90m)")
    (spend (minutes 90)
        . estimate (hours 2)
        . task
        $ "do this")

testTimed2 :: Test
testTimed2 = rightOrFail (parseTask "do this (2h 30m,90m)")
    (spend (minutes 90)
        . estimate (hours 2 + minutes 30)
        . task
        $ "do this")

testTimed3 :: Test
testTimed3 = rightOrFail (parseTask "do this (2:30,90m)")
    (spend (minutes 90)
        . estimate (hours 2 + minutes 30)
        . task
        $ "do this")

testTimed4 :: Test
testTimed4 = rightOrFail (parseTask "do this (2:30,90m) again")
    (spend (minutes 90)
        . estimate (hours 2 + minutes 30)
        . task
        $ "do this again")

testStarted :: Test
testStarted = rightOrFail
    (parseTask "do this (2h, 90m) {02/11/2014 12:20}")
    (start (LocalTime (fromGregorian 2014 2 11) (TimeOfDay 12 20 0))
        . spend (minutes 90)
        . estimate (hours 2)
        . task
        $ "do this")
