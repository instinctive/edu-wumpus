{-# LANGUAGE NoImplicitPrelude #-}

module Input where

import BasePrelude
import qualified System.IO as Sys

import Common
import Output
import Parse

-- | Get an 'Action' from the player.
--
-- This function just calls 'hGetAction' on 'Sys.stdin'.
getAction :: IO Action
getAction = hGetAction Sys.stdin

-- | Get an 'Action' from the player.
--
-- This function should prompt the user using 'printPrompt', and then use
-- 'hGetLine' and 'parseAction to get the player action.
--
-- Note that 'hGetLine' is just like 'getLine' except that it takes a 'Handle'
-- argument, which will be @h@ in this case. We write the function with this
-- extra handle argument so that we can test it.
--
-- Note that 'parseAction' has a 'Maybe' return type, and this function does
-- not. If the parsed action is 'Nothing', this function should use 'printHelp'
-- to print the help text, and then try again.
--
-- >>> test i = Sys.openTempFile "/tmp" "foo" >>= \(s,h) -> Sys.hPutStrLn h i >> Sys.hClose h >> pure s >> Sys.withFile s Sys.ReadMode hGetAction
-- >>> test "move 4"
-- What do you do? Move 4
-- >>> test "shoot 4"
-- What do you do? Shoot 4
-- >>> test "sdfsdf\nmove 4"
-- What do you do? Valid commands:
--   quit
--   move <cave>
--   shoot <cave>
-- What do you do? Move 4
hGetAction :: Sys.Handle -> IO Action
hGetAction h = undefined
