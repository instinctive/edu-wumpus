module Main where

import System.IO (hFlush, hPutStrLn, stderr)
import Test.DocTest (doctest)

main :: IO ()
main = do
    hPutStrLn stderr "\nStarting doctests...\n"
    test "src/Cave.hs"
    -- test "src/Common.hs"
    -- test "src/Game.hs"
    -- test "src/Output.hs"
    -- test "src/Parse.hs"
    -- test "src/Input.hs"
    -- test "src/Play.hs"
  where
    test path = do
        hPutStrLn stderr $ "Testing " ++ show path
        hFlush stderr
        doctest [path]

