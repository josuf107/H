module H where

import H.Data
import H.Parse

run :: IO ()
run = do
    putStrLn "H\n\n"
    putStrLn "Enter task: "
    t <- getLine
    case fmap encode (parseTask t) of
        (Right s) -> putStrLn s
        (Left e) -> print e
